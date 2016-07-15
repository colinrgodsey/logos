package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.{ExtraMath, RingSort}

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



/*
random thought on recursive architecture -

Considering the often recursive nature of biology-
 * Each cell (in relation to its distal dendrites) could be considered a
   layer with a desired activity of 1. Columns ~== distal dendrites.
 * Each cell in a column could be considered a column in a layer (with added 'bursting mechanic', maybe akin to boosting).
 * Each dendrite could be considered a layer with only 1 column, or just a column.
 * Layer with 1 column ~== column

 */

trait LayerColumnTemporalPooler extends SynapseReinforcement {
  val config: MutableLayerState.Config
  import config._

  def layerState: MutableLayerState

  def regionWidth: Int
  def maxDistal: Int
  def cellDepth: Int
  def learningCellTickDuration: Byte
  def connectionThreshold: Int
  def activationThreshold: Int

  def getLastActivation(nodeIdx: Int): Boolean

  //resolves the cell indexes 'above' the column node idx
  def getCellIdx(columnIdx: Int, cellDepthIndex: Int): Int =
    columnIdx - (cellDepthIndex + 1) * regionWidth

  def onColumnActivate(columnIdx: Int): Unit = {
    selectAndReinforceLearningCell(columnIdx)
  }

  //TODO: any way to de-dupe this?
  def getDistalSegmentOverlap(nodeIdx: Int, segmentIdx: Int): Int = {
    var numActive = 0
    var hashAcc = 0x1071E //some seed

    for(connectionIdx <- 0 until maxConnections) {
      val inputIdx = layerState.getConnectionTargetIdx(nodeIdx, segmentIdx, connectionIdx)
      val inputPerm = layerState.getConnectionTargetPermanence(nodeIdx, segmentIdx, connectionIdx)

      //reference last activate state here
      if(inputIdx != 0 &&
          inputPerm >= connectionThreshold &&
          getLastActivation(inputIdx)) {
        //take hash of inputs and eventually columnIdx for ordinal
        hashAcc = MurmurHash3.mix(hashAcc, inputIdx)
        numActive += 1
      }
    }

    //TODO: FIX! if no active synapses, hash ordinal is constant and not based on input

    //the important part- mix our node id in
    hashAcc = MurmurHash3.mixLast(hashAcc, nodeIdx)
    //and segment id
    hashAcc = MurmurHash3.mixLast(hashAcc, segmentIdx)

    val ordinal = MurmurHash3.finalizeHash(hashAcc, numActive + 2) & 0xFF

    if(numActive >= activationThreshold) {
      //ordinal used as tie break. deterministic based on (activeInputIds, columnId).hashCode
      (numActive << 8) | ordinal
    } else ordinal
  }

  /*
  Called when a segment is selected for learning on a non-predictive 'bursting' scenario.
  Destroys connection data and seeds from 'prediction' map of learning cells
   */
  def seedDistalSynapse(cellIdx: Int, segmentIdx: Int): Unit = ???

  /*
  Select the best predicted learning cell and reinforce its distal dendrite.
  This is essentially the temporal pooling operation. Selects learning cell, reinforces
  distal synapses, activates cells, sets learning cell on prediction map
   */
  def selectAndReinforceLearningCell(columnIdx: Int): Unit = {
    var maxCellIdx = 0
    var maxSegmentIdx = 0
    var maxOverlap = 0

    //select learning cell and distal segment
    for {
      cellDepthIndex <- 0 until cellDepth
      segmentIdx <- 0 until maxDistal
    } {
      val cellIdx = getCellIdx(columnIdx, cellDepthIndex)
      val overlap = getDistalSegmentOverlap(cellIdx, segmentIdx)

      if(overlap > maxOverlap) {
        maxCellIdx = cellIdx
        maxSegmentIdx = segmentIdx
        maxOverlap = overlap
      }
    }

    //if our overlap is larger than the ordinal range, a segment 'fired'
    val wasPredicted = maxOverlap > 0xFF

    //reset cell activation to 0 for predicted, 1 for bursting
    for(cellDepthIndex <- 0 until cellDepth) {
      val cellIdx = getCellIdx(columnIdx, cellDepthIndex)

      if(wasPredicted)
        layerState.setActivation(cellIdx, 0)
      else //bursting
        layerState.setActivation(cellIdx, 1)
    }

    //set as learning cell for next frame prediction
    layerState.setPrediction(maxCellIdx, true)

    //set learning cell activation
    if(wasPredicted) {
      reinforce(maxCellIdx, maxSegmentIdx) //reinforce existing learning segment
      //TODO: maybe add synapse?
      layerState.setActivation(maxCellIdx, learningCellTickDuration)
    } else { //bursting, cell/distal selected for leaning

    }
  }
}

/*
thoughts on boost:

2 types of boost:

Column could activate via FF input but is inhibited---
* accumulated 'boost' to overlap score
* lets less-overlapping columns fire at a lower frequency/chance than better matching ones
  * useful noise? almost preparing 'backup' columns
  * doesnt pollute local activation that much as it resets the first time its not inhibited

Column does not activate enough via FF input---
* boost synapse strength until something meaningful happens
* creates fully-connected proximal connections for unused/inactive sections of input....
  * natural 'seeding' of synaptic process
  * should proximal start fully connected?
  * could destroy meaningful synaptic data
* could happen after a tick-domain 'TTL' or as part of GC?
  * could start to increase after so many ticks of inactivity
  * could happen in response to 'stress' or other global modulation
    * stress could globally modulate the rate at which inactive cells 'GC'
    * this could be a natural way to manage 'memory capacity'. stressful
      situations could be a call for 'more capacity' and maybe a way instinctual drive
      calls for higher-level processing. columns wouldnt normally 'arbitrarily' compete (synaptically)
      unless there was a global indicator signaling distress. this would solidify important
      and often used columns while leaving others up to experimentation and competition.
    * would have to be weighted on some combination of 'total synaptic weight' and activity,
      this would preserve more of the very solid connections formed.
    * could stress be caused by column 'bursting' ?

distal synapse selection:
  * rate at which new synapses are added to a distal dendrite?
  *
duty cycle:
  * capture/reset frame counters
 */

object LayerSpatialPooler {
  trait Config extends MutableLayerState.Config {
    def boostDivisor: Int = 0xFF //frames per activation point, limit max 1 point for 0xFFFF, max FF points for 0xFF, recommend FF
    def connectionThreshold: Int
    def activationThreshold: Int
    def desiredLocalActivity: Int
    def inputStdDevInputPercent: Double// = 0.8 //percent of input range thats 68.2% input to column
    def stdDev: Double// = layerState.inputLength * inputStdDevInputPercent / 2.0
  }
}

//TODO: both types of boost
trait LayerSpatialPooler extends SynapseReinforcement {
  val config: LayerSpatialPooler.Config
  import config._

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

  /* stores boost values for column. should this also start out random to prevent events? */
  val boostBuffer: Array[Short]

  //needs to start out random so we dont have regional strengthening events/bursts!!
  val dutyCycle: Array[Short]

  def layerState: MutableLayerState

  //incorporates radius, topology, etc
  def getInhibitionNeighborLocalIdx(localColumnIdx: Int): Iterator[Int] //TODO: interator

  def onColumnActivate(columnIdx: Int): Unit

  //location here is from [0 to 1) for entire column range in layer
  def selectProximalInputs(columnIdx0: Int, inputIndices: IndexedSeq[Int]): Iterable[Int] = {
    val columnLocation: Double = columnIdx0 / regionWidth

    val middleInputIdx = (columnLocation * inputIndices.length).toInt

    def normalize(idx: Int): Int = {
      val maxIdx = inputIndices.length
      var x = idx

      while(x < 0) x += maxIdx
      while(x >= maxIdx) x -= maxIdx

      x
    }

    def nextDouble = math.random

    //intentionally let this wrap for 2x coverage!
    //could produce 2x as many samples as input length
    val itr = (0 until inputIndices.length).iterator flatMap { dx =>
      //probability for the domain of [x, x + dx]
      val probability = ExtraMath.normalCDF(dx, 1, 0, stdDev)

      val idxLeft = normalize(middleInputIdx - dx)
      val idxRight = normalize(middleInputIdx + dx)

      //take a sample for left/right dx with given probability
      val selectLeft = nextDouble < probability
      val selectRight = nextDouble < probability

      val left = if(selectLeft) List(idxLeft) else Nil
      val right = if(selectRight) List(idxRight) else Nil

      left ++ right
    }

    itr.toStream.distinct take maxConnections
  }

  def seedColumnProximal(columnIdx: Int, inputIndices: IndexedSeq[Int]): Unit = {
    val loc = columnIdx

    layerState
  }

  /*
  thoughts on duty cycle 'synaptic strengthening'

  columns that never activate either:
    * started out fully connected to something.
    * connect to unused inputs via synapse/potential synapse
    * weights optimized to input pattern that has not been seen again
    * could ALWAYS potentially activate for some input (connected synapses always >= activation threshold)
    *
    * column states:
    * primed - fully connected
    * specialized - some unused synapses disconnect, and become potential
    * stale - specialized state that has become unused
    *
    * Every proximal pattern starts primed and has the potential to go stale.
    * No way to determine if a stale column is more important than a specialized one.
    * Must GC based on usage... but must be done sparingly as we could destroy specialized data.
    *
    * Doesnt necessarily destroy data like i said before!! It just de-specializes them slightly and
    * lets other patterns potentially fire and re-specialize them.
    *
    * **** Should we just consider FF specialization potentially 'weak' ?
    *
    * Track duty cycle for a number of frames, boost synapses on some small percentage.
    * Number of frames for sample - sample period
    *
    * Number of frames in sample period must be <= max prim value use for tracking
    * Strengthening threshold- if column is in lower % activity, strengthen.
    *
    * Counters - track nonDutyCycle - 1 count per frame of inactivity. after X frames
   */

  def columnarInhibition(): Unit = {
    //all columns with a non-0 overlap
    for(columnIdx0 <- 0 until regionWidth; if overlapBuffer(columnIdx0) > 0) {
      val columnIdx = columnIdx0 + layerState.columnIndexOffset
      val overlap = overlapBuffer(columnIdx0)

      val oldDutyCycle = dutyCycle(columnIdx0)
      val newDutyCycle = (oldDutyCycle + 1).toShort

      if(newDutyCycle > oldDutyCycle) //check overflow
        dutyCycle(columnIdx0) = newDutyCycle

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

        reinforce(columnIdx, 0)
        boostBuffer(columnIdx0) = 0 //clear boost points
        onColumnActivate(columnIdx)
      } else {
        //add boost point for 1 'inhibited frame'
        val oldValue = boostBuffer(columnIdx0)
        val newBoost = (oldValue + 1).toShort

        //make sure we didnt int overflow here
        if(newBoost > oldValue)
          boostBuffer(columnIdx0) = newBoost
      }
    }
  }

  /*
  thoughts on ordinal:
    * THE WINNER IS!-- deterministic (setOfActiveConnectedNodeIdx, columnIdx/distalIdx).hashCode
    *
    * deterministic leader for every distinct set of input activations
    *
    * biological justification- naturally slow/never changing differences in inhibitory
    * connection strength (meshed interconnected inhibitory)?
    *   * some columns naturally win over OTHER COLUMNS...
    *   * does this make sense with the ordinal? maybe hash of (thisColumn, thatColumn).## is fine for sort ordinal?
    *   * TODO: figure out if these equate, and/or what the difference is. roughly both select a leader for a distinct set of spatial activation
    *
    *
    * Old ideas:
    *
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
  def calculateColumnOverlap(): Unit = {
    for(columnIdx0 <- 0 until regionWidth) {
      val columnIdx = columnIdx0 + layerState.columnIndexOffset
      var numActive = 0
      var hashAcc = 0x1231A //some seed

      for(connectionIdx <- 0 until maxConnections) {
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
        val boostPoints = boostBuffer(columnIdx0) / boostDivisor

        //ordinal used as tie break. deterministic based on (activeInputIds, columnId).hashCode
        overlapBuffer(columnIdx0) = (numActive << 8) | (ordinal & 0xFF) + boostPoints
      }

      //need to explicitly unset, deactivations normally handled by countdown timer for cells
      layerState.setActivation(columnIdx, 0)
    }
  }
}

//TODO: should this be moved into MutableLayerState?
trait SynapseReinforcement {
  val config: MutableLayerState.Config
  import config._

  def layerState: MutableLayerState
  def permanenceIncr: Int
  def permanenceDecr: Int

  //calculate average receptive field size here too
  def reinforce(nodeIdx: Int, segmentIdx: Int): Unit = {
    for(connectionIdx <- 0 until maxConnections) {
      val inputIdx = layerState.getConnectionTargetIdx(nodeIdx, segmentIdx, connectionIdx)
      val inputPerm = layerState.getConnectionTargetPermanence(nodeIdx, segmentIdx, connectionIdx)

      if(inputIdx != 0) {
        val isActive = layerState.getActivation(inputIdx)

        if(isActive)
          layerState.addConnectionTargetPermanence(nodeIdx, segmentIdx, connectionIdx, permanenceIncr)
        else
          layerState.addConnectionTargetPermanence(nodeIdx, segmentIdx, connectionIdx, permanenceDecr)
      }
    }
  }
}

object MutableLayerState {
  trait Config {
    def regionWidth: Int
    def cellDepth: Int
    def maxConnections: Int
    def maxDistal: Int
  }
}

case class LayerSpaceConfiguration(regionWidth: Int,
    cellDepth: Int, maxConnections: Int, maxDistal: Int) extends MutableLayerState.Config {
  val numCells = regionWidth * cellDepth
  val numColumns = regionWidth

  //for connections
  val segmentConnectionSize = maxConnections * 2
  val cellConnectionSize = maxDistal * segmentConnectionSize
  val inputConnectionOffset = numCells * cellConnectionSize
  val connectionIntSize = (numCells * maxDistal + numColumns) * maxConnections * 2
}

case class LayerAllocationConfiguration(
    spaceConfig: LayerSpaceConfiguration, sharedIndexOffset: Int) {
  import spaceConfig._

  val maxIdx = sharedIndexOffset + numCells + numColumns
  val columnIndexOffset = sharedIndexOffset + numCells
}

abstract class MutableLayerState(config: LayerSpaceConfiguration) extends LayerState { self =>
  import config._

  def inputActivationIndex: Int //offset in activation data where we get input
  //NOTE: recommended input length should be ~ the number of cells (overall SDR params will match w distal and proximal)
  def inputLength: Int //length of that data from offset
  def sharedIndexOffset: Int //where cell activations start followed by column activations

  def setPrediction(nodeIdx: Int, value: Boolean): Unit


  //this can always be local to the layer
  val connection = new Array[Int](connectionIntSize) //(ID: Int, permanence: Int)

  def maxIdx = sharedIndexOffset + numCells + numColumns
  def columnIndexOffset = sharedIndexOffset + numCells

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
