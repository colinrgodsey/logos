package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RingSort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

//TODO: entire region data should be kept together via... data views?

/*
Region state is built in one iteration. These iterations do not reference the buffer being written, instead reference
the previous buffer, creating a function nextState(currentState: State): State.

1. Evaluate external input spatial layers
2. Evaluate feed-forward column activation (column willActivate vs stateful column active)
3. Evaluate predictive cell state in active columns (local wasPredicted) referencing 'cell active'
4. Evaluate cell activation in column (cell willActivate)
5. Evaluate distal predictive connections using 'cell active' (current untouched state) on  selected 'learning node'
 */

trait LayerState {
  //TODO: prediction is the only thing that needs 'old' references! activation is per-frame FF only
  /**
   * Activation is always read in a feed-forward way from the current pending state. This state
    * is meant to be discarded after each frame output. */
  private[cla] val activation: Array[Byte] //shared

  /**
   * Connection is an iterative state that is preserved and mutable through each frame.
   */
  private[cla] val connection: Array[Int]

  /**
   * Prediction is always written to current state, read from previous state.
    * This selects the 'learning' cells out of any activation range. This range
    * is copied after the frame and used for learning cell reference in the next frame.
    * It is then cleared for the new frame.
    * */
  private[cla] def getPrediction(nodeIdx: Int): Boolean //shared, previous state

  def numCells: Int
  def inputConnectionOffset: Int
  def cellConnectionSize: Int
  def segmentConnectionSize: Int

  def getActivation(nodeIdx: Int): Boolean = activation(nodeIdx) != 0

  @inline final protected def getConnectionOffset(nodeIdx: Int, segmentIdx: Int, connectionIdx: Int): Int =
    if (nodeIdx >= numCells)
      inputConnectionOffset + nodeIdx * segmentConnectionSize //input connections
    else
      nodeIdx * cellConnectionSize

  def getConnectionTargetIdx(nodeIdx: Int, segmentIdx: Int, connectionIdx: Int): Int = {
    val idx = getConnectionOffset(nodeIdx, segmentIdx, connectionIdx)

    connection(idx)
  }

  def getConnectionTargetPermanence(nodeIdx: Int, segmentIdx: Int, connectionIdx: Int): Int = {
    val idx = getConnectionOffset(nodeIdx, segmentIdx, connectionIdx)

    connection(idx + 1)
  }
}


sealed trait ImmutableRegionState extends LayerState

/*

activation map, bytes, ticked down 3,2,1,0 etc.
feed forward space can be special copy-to registers, or activation in the SAME state elsewhere.
basically, if choosing to allocate space for FF input, you are copying the values into the current state.
by feed-forward nature these are referenced in a linear process from the current state; order of operations
must be considered to maintain state integrity.
CCCCCCC
CCCCCCC   cell activation
IIIIIII   column activation
DDDDDDDDD feed forward input registers, not same width as region, tail end only, not really nodes

prediction map, boolean, used to mark learning cell/nodes:
PPPPPPP
PPPPPPP
PPPPPPP   column activation can possibly be used for learning
xxxxxxxxx not allocated, unused for feedforward registers

InputSDR equivalent is a layer with no cells.

connection map, Array[Int] Tuple of node-id and int 'strength' 0 - IntMax
node-0 is default, marks no node. sorry node 0.
length = (numCell * maxDistal + regionWidth) * maxConnections * 2
CCCCCCC
CCCCCCC
  ...     x maxDistal
IIIIIII
 */


/*
regional state has to swap, layer is a view on that state plus extra connection state.
sptial/temporal pooling has to happen per layer in sequence in order for self-referencing state to work

width must be uniform across layers so we can move 'up' in the data vector. def up(idx) = idx - width

CCCCCCC
CCCCCCC
CCCCCCC
IIIIIII
CCCCCCC
CCCCCCC
IIIIIII
CCCCCCC
IIIIIII
DDDDDDDDDDDDDD

input space is mapped to the cell space of another layer, or somewhere in the 'D' regional input space
 */

class SomeTestRegion { region =>
  val regionWidth = 128
  val globalInputLength = 100
  val boostDivisor: Int = ???
  val permanenceDecr: Int = ???
  val permanenceIncr: Int = ???
  val desiredLocalActivity: Int = ???
  val connectionThreshold: Int = ???
  val activationThreshold: Int = ???

  //these 2 states are entirely mutable and never re-allocated
  val currentState: RegionState = ???
  val lastState: RegionState = ???

  trait SharedState {
    def setPrediction(nodeIdx: Int, value: Boolean): Unit = currentState.prediction(nodeIdx: Int) = value

    /**
     * Prediction is always written to current state, read from previous state.
     * This selects the 'learning' cells out of any activation range. This range
     * is copied after the frame and used for learning cell reference in the next frame.
     * It is then cleared for the new frame.
     **/
    private[cla] def getPrediction(nodeIdx: Int): Boolean = lastState.prediction(nodeIdx)

    private[cla] val activation: Array[Byte] = currentState.activation
  }

  val Layer1 = new MutableLayerState(
    regionWidth = regionWidth,
    cellDepth = 8,
    maxConnections = 16,
    maxDistal = 16
  ) with SharedState with LocalRegionalLayer {
    val overlapBuffer = new Array[Short](regionWidth)
    val ordinalBuffer = new Array[Byte](regionWidth)

    val sharedIndexOffset = 0

    def inputActivationIndex: Int = globalInputIndex
    def inputLength: Int = globalInputLength
  }

  val Layer2 = new MutableLayerState(
    regionWidth = regionWidth,
    cellDepth = 8,
    maxConnections = 16,
    maxDistal = 16
  ) with SharedState with LocalRegionalLayer {
    val overlapBuffer = new Array[Short](regionWidth)
    val ordinalBuffer = new Array[Byte](regionWidth)

    val sharedIndexOffset = Layer1.maxIdx

    def inputActivationIndex: Int = Layer1.sharedIndexOffset
    def inputLength: Int = Layer1.numCells
  }

  val totalActivationLength = Layer2.maxIdx
  val globalInputIndex = totalActivationLength
  val totalNodeLength = globalInputIndex + globalInputLength

  /**
   * Copy current state to 'last state' and decrement activations in current state
   */
  def swapState(): Unit = {
    for(idx <- 0 until currentState.activation.length) {
      val curActivation = currentState.activation(idx)

      lastState.activation(idx) = curActivation

      if(curActivation > 0) currentState.activation(idx) = (curActivation - 1).toByte

      lastState.prediction(idx) = currentState.prediction(idx)

      currentState.prediction(idx) = false
    }
  }

  class RegionState {
    val activation = new Array[Byte](totalActivationLength)
    val prediction = new Array[Boolean](totalActivationLength)
  }
  
  trait LocalRegionalLayer extends LayerSpatialPooler with LayerColumnUpdate { _: MutableLayerState =>
    def layerState: MutableLayerState = this

    def regionWidth: Int = region.regionWidth
    def boostDivisor: Int = region.boostDivisor
    def permanenceDecr: Int = region.permanenceDecr
    def permanenceIncr: Int = region.permanenceIncr
    def desiredLocalActivity: Int = region.desiredLocalActivity

    def connectionThreshold: Int = region.connectionThreshold
    def activationThreshold: Int = region.activationThreshold

    //incorporates radius, topology, etc
    def getInhibitionNeighborLocalIdx(localColumnIdx: Int): Iterator[Int] = ???
  }
}

trait LayerColumnUpdate {
  def layerState: MutableLayerState
  //def previousState: LayerState

  def connectionThreshold: Int
  def activationThreshold: Int

  /*
  contains 24-bit overlap (O) and 8-bit ordinal (T) data
  OOOOOOOOOOOOTTTT 32 bits
   */
  val overlapBuffer: Array[Int]

  /*
  this is the important hash-based ordinal buffer. keeps a hash of column
  idx and active input idx for deterministic 'tie-breaking'
   */
  val ordinalBuffer: Array[Byte]

  /*
  thoughts on ordinal:
    * integer overlap is hard to compare
    * using spatial pooling cant actually control the amount of activity
    * local activity actually gets higher when columns have similar activity
    * any reasonably varied fractional difference at all could help, something deterministic but based on input.
      * one good option- (Seq(activeInputIndex...), columnIdx).hashCode. this creates a 'natural leader' for a specific set of input
    * you can use any deterministic method: traversable order, static ordinal, hashcode. end result
      is that for any one set of inputs, any columns activated by it with equal overlap will have
      the same winner every time.
    * you can make it random, but makes learning very noisy
    * can use permanence value in overlap sum- against CLA, could result in overfitting
  */
  def update(): Unit = {
    for(columnIdx0 <- 0 until layerState.regionWidth) {
      val columnIdx = columnIdx0 + layerState.sharedIndexOffset
      var numActive = 0
      var hashAcc = 0x1231A //some seed

      for(connectionIdx <- 0 until layerState.maxConnections) {
        val inputIdx = layerState.getConnectionTargetIdx(columnIdx, 0, connectionIdx)
        val inputPerm = layerState.getConnectionTargetPermanence(columnIdx, 0, connectionIdx)

        if(inputIdx != 0 &&
            inputPerm >= connectionThreshold &&
            layerState.getActivation(inputIdx)) {
          //take hash of inputs and eventually columnIdx for ordinal
          hashAcc = MurmurHash3.mix(hashAcc, inputIdx)
          numActive += 1
        }
      }

      if(numActive >= activationThreshold) {
        //the important part- mix our column id in
        hashAcc = MurmurHash3.mixLast(hashAcc, columnIdx)

        val ordinal = MurmurHash3.finalizeHash(hashAcc, numActive + 1)

        //ordinal used as tie break. deterministic based on (activeInputIds, columnId).hashCode
        overlapBuffer(columnIdx0) = (numActive << 8) | (ordinal & 0xFF)
      }

      //need to explicitly unset, deactivations normally handled by countdown timer for cells
      layerState.setActivation(columnIdx, 0)
    }
  }
}

//TODO: both types of boost
trait LayerSpatialPooler {
  def layerState: MutableLayerState

  def boostDivisor: Int //these are integer overlaps, integer divisor

  def desiredLocalActivity: Int
  def permanenceIncr: Int
  def permanenceDecr: Int

  //incorporates radius, topology, etc
  def getInhibitionNeighborLocalIdx(localColumnIdx: Int): Iterator[Int] //TODO: interator

  val overlapBuffer: Array[Short]
  val ordinalBuffer: Array[Byte]

  //calculate average receptive field size here too
  def reinforce(columnIdx: Int): Unit = {
    for(connectionIdx <- 0 until layerState.maxConnections) {
      val inputIdx = layerState.getConnectionTargetIdx(columnIdx, 0, connectionIdx)
      val inputPerm = layerState.getConnectionTargetPermanence(columnIdx, 0, connectionIdx)

      if(inputIdx != 0) {
        val isActive = layerState.getActivation(inputIdx)

        if(isActive)
          layerState.addConnectionTargetPermanence(columnIdx, 0, connectionIdx, permanenceIncr)
        else
          layerState.addConnectionTargetPermanence(columnIdx, 0, connectionIdx, permanenceDecr)
      }
    }
  }

  def update(): Unit = {
    //all columns with a non-0 overlap
    for(columnIdx0 <- 0 until layerState.regionWidth; if overlapBuffer(columnIdx0) > 0) {
      val columnIdx = columnIdx0 + layerState.sharedIndexOffset
      val overlap = overlapBuffer(columnIdx0)

      val neighbors = getInhibitionNeighborLocalIdx(columnIdx0).map { idx =>
        val overlap = overlapBuffer(idx)
        /*
        append ordinal here and sort as int
        The 8 'fractional' bits of the overlap are used during a tie to select a deterministic leader
        for a specific set of active inputs.
         */
        if(overlap != 0) (overlap.toInt << 8) | (ordinalBuffer(idx).toInt & 0xFF)
        else 0
      }

      //TODO: ditch vector, make pimp for Iterator#last
      val top = RingSort.takeTop(desiredLocalActivity, neighbors).toVector

      //activate if our overlap is within the top k
      if(top.last <= overlap) { //activate
        layerState.setActivation(columnIdx, 1)

        reinforce(columnIdx)
      }
    }
  }
}

abstract class MutableLayerState(
  val regionWidth: Int,
  val cellDepth: Int,
  val maxConnections: Int,
  val maxDistal: Int
) extends LayerState { self =>
  def inputActivationIndex: Int //offset in activation data where we get input
  def inputLength: Int //length of that data from offset
  def sharedIndexOffset: Int //where cell activations start followed by column activations

  def setPrediction(nodeIdx: Int, value: Boolean): Unit
  
  val numCells = regionWidth * cellDepth
  val numColumns = regionWidth

  //for connections
  val segmentConnectionSize = maxConnections * 2
  val cellConnectionSize = maxDistal * segmentConnectionSize
  val inputConnectionOffset = numCells * cellConnectionSize

  //this can always be local to the layer
  val connection = new Array[Int]((numCells * maxDistal + numColumns) * maxConnections * 2) //(ID: Int, permanence: Int)

  def maxIdx = sharedIndexOffset + numCells + numColumns

  def setConnectionTargetIdx(nodeIdx: Int, segmentIdx: Int, connectionIdx: Int, targetIdx: Int): Unit = {
    val idx = getConnectionOffset(nodeIdx, segmentIdx, connectionIdx)

    connection(idx) = targetIdx
  }

  def addConnectionTargetPermanence(nodeIdx: Int, segmentIdx: Int, connectionIdx: Int, permanenceDelta: Int): Unit = {
    val idx = getConnectionOffset(nodeIdx, segmentIdx, connectionIdx) + 1
    val current = connection(idx)
    val newValue = current + permanenceDelta

    val result: Int =
      if(newValue < 0 && permanenceDelta < 0) 0
      else if(newValue < current && permanenceDelta > 0) Int.MaxValue //overflowed to less than starting
      else if(newValue > current && permanenceDelta < 0) 0 //underflowed to more than starting
      else newValue

    connection(idx) = result
  }

  def setActivation(nodeIdx: Int, value: Byte): Unit = {
    require(value >= 0)

    if(activation(nodeIdx) != value)
      activation(nodeIdx) = value
  }

  /*def decrActivation(nodeIdx: Int): Unit = {
    val cur = activation(nodeIdx)

    if(cur != 0) activation(nodeIdx) = (cur - 1).toByte
  }*/
}

//START HERE!!
trait SingleL3Layer {


  //flip-flop states: old lastState copies from pendingState, lastState swaps to pendingState, then process.
  def pendingState: MutableLayerState
  def lastState: LayerState

  def copyInputs(): Unit //copy inputs into input registers

  def update(): Unit = {
    copyInputs()


  }
}
