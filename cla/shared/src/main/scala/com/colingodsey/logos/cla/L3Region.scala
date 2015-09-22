package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.ExtraMath.randomNormal
import com.colingodsey.logos.collections.RollingAverage

import scala.collection.mutable
import scala.concurrent.ExecutionContext

trait Region {
  //protected type LocalL3Layer <: L3Layer
}


/*
TODO: note on goals: in regions of the brain that can effect behavior, dopamine will strengthen
or weakean the active connections. prediction of dopamine must somehow also produce dopamine,
 ala q-learning recursive reward 'spreading'.
 the idea of a reward is rewarding until proven otherwise.

 This is probably done in layer 5.

 Most likely:
 Associates incoming motor commands with motor 'triggers'.
 Associates l3 state with 'rewarding' motor triggers.

 Ultimately must mimic actions of rewarding instinctual action.

 OR (or and/or)

 l5 has arbitrary inputs into the old brain, and uses dopamine to train old brain to respond to
 its outputs (no mimicry of 'triggers'). this can probably be done by just firing the l5
 nodes at the same time as the original reward? natural effect of being weakened if action
 was mislearned and does not lead to a reward.

  during times of dopamine, patterns are more expected.

  could maybe be done with an SDR that has a way of activating columns if predicted,
  but have no feedforward. if strongly predicted, feedforward could activate. possibly
  some special mechanic of "if strongly predicted but not active, feed this SDR forward"

  layer 2 of the 2/3 layer probably has cell(s) that receive feedback from higher region.

  IMPORTANT!!:
  these types of feedback of special input cells basically act as feed-forward input
  by activating a cell in a column (even if its not active from feed forward), but provides
  a single order sequence as it cant burst the column. layer 2 may use this for feedback,
  and layer 4 may use this for sensory input (instead of offering as distal nodes). in layer 4,
  the learning node may be fixed as the sensory node (maybe?)?
 */

//TODO: for apical dendrites, maybe represent those as just one cell in a column

//TODO: higher layers are essentially filters for feedback control?

class L3Region[L](implicit val config: CLA.Config[L]) extends Region { region =>
  import CLA._
  import config._

  val sensoryInput = new InputSDR[L]

  object l3Layer extends InternalLearningLayer[L] { l3Layer =>
    implicit val config = region.config

    val id = "l3"

    val columns: IndexedSeq[Column[L]] =
      (0 until sensoryInput.segments.length).map { idx =>
        val segment = sensoryInput.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](l3Layer, loc, segment)
      }.toIndexedSeq

    def update(): Unit = {
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
  }

  def update(input: Input): Unit = {
    sensoryInput.update(input) //spatial pooling
    l3Layer.update()


  }
}

//TODO: columns that attach to 'special' input have a single cell that always activates. this reduces the context of this input to be effectively 1st order
//when in doubt- use as proximal inputs because you'll get distal nodes in the column

//to mix inputs, simply interlace with even spacing (width of 8 is 'stretched' so it can interlace with a width of 16)
//so scale all inputs to same width, total width is sum of original widths

//by maintain locality of proximal inputs, feedback and others can 'mirror' a set of sensory inputs in a way
final case class MixInputs(inputs: CLA.InputSource*) extends CLA.InputSource {
  val width = inputs.iterator.map(_.width).sum

  private class InputCursor(input: CLA.InputSource) {
    val stride = input.width.toDouble / width
    val itr = input.iterator

    var cursor = 0.0

    def tick(): Unit = cursor -= stride

    def hasNext = itr.hasNext
    def next() = {
      cursor += 1
      itr.next()
    }
  }

  def iterator = new Iterator[Boolean] {
    private val cursors = inputs.iterator.map(new InputCursor(_)).toArray

    tick()

    @inline private def activeCursors = cursors.iterator.filter(_.hasNext)
    @inline private def tick(): Unit = cursors.foreach(_.tick())

    final def next() = {
      val lowest = activeCursors.minBy(_.cursor)

      tick()

      lowest.next()
    }

    def hasNext = activeCursors.hasNext
  }
}

class FullRegion[L](implicit val config: CLA.Config[L]) extends Region { region =>
  import CLA._
  import config._

  val numl4cells = 8
  
  val motorInput = new InputSDR[L]
  val sensoryInput = new InputSDR[L]

  object l6Layer extends ExternalLearningLayer[L] {
    implicit val config = region.config.copy(columnHeight = numl4cells,
      maxDistalDendrites = maxDistalDendrites / 4)

    val id = "l6"

    //TODO: this l5 -> l6 ff loop probably doesnt exist.
    //lazy val l5SDR = new InputSDR[L](l5Layer)
    //lazy val inputSource = MixInputs(l5SDR, motorInput)
    lazy val inputLayer = motorInput//new InputSDR[L](inputSource)

    lazy val columns: IndexedSeq[Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](this, loc, segment)
      }.toIndexedSeq

    def extraLayerLearningNodes: Stream[NeuralNode] = l5Layer.getLearningNodes

    def update(): Unit = {
      //l5SDR.update(l5Layer)
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }
  }

  object l5Layer extends InternalLearningLayer[L] { //ExternalLearningLayer[L] {
    implicit val config = region.config.copy(columnHeight = numl4cells,
      maxDistalDendrites = maxDistalDendrites / 4)

    lazy val l3SDR = new InputSDR[L](l3Layer)

    lazy val inputSource = MixInputs(l3SDR, motorInput)
    lazy val inputLayer = new InputSDR[L](inputSource)

    val id = "l5"

    lazy val columns: IndexedSeq[Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](this, loc, segment)
      }.toIndexedSeq

    def update(): Unit = {
      l3SDR.update(l3Layer)
      inputLayer.update(inputSource.produce)
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }
  }

  object l4Layer extends ExternalLearningLayer[L] {
    implicit val config = region.config.copy(
      overlapPercent = region.config.overlapPercent / 2,
      columnHeight = numl4cells, maxDistalDendrites = maxDistalDendrites / 4)

    //TODO: l6 as distal, spiny stellate as 'invisible' learning cell, spiny stellate has thala ff
    lazy val l6SDR = new InputSDR[L](l6Layer)

    //l6 may not be feedforward?
    //lazy val inputSource = MixInputs(sensoryInput, motorInput, l6SDR)
    lazy val inputSource = MixInputs(sensoryInput, motorInput)
    lazy val inputLayer = new InputSDR[L](inputSource)
    
    val id = "l4"

    lazy val columns: IndexedSeq[Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](this, loc, segment)
      }.toIndexedSeq

    def extraLayerLearningNodes: Stream[NeuralNode] = {
      region.motorInput.segments.toStream.filter(_.wasActive).sortBy(_.activationOrdinal).reverse ++
          l6Layer.getLearningNodes
    }

    override def requiresExtraLayerNodes = true

    def update(): Unit = {
      l6SDR.update(l6Layer)
      //if(math.random < 0.5) println(inputSource.produce.map(x => if(x) 1 else 0).mkString)
      inputLayer.update(inputSource.produce)
      //inputLayer.update(sensoryInput.produce)
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }
  }

  object l3Layer extends InternalLearningLayer[L] {
    implicit val config = region.config.copy()

    lazy val inputLayer = new InputSDR[L](l4Layer)

    val id = "l3"

    lazy val columns: IndexedSeq[Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](this, loc, segment)
      }.toIndexedSeq

    def update(): Unit = {
      inputLayer.update(l4Layer)
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
  }

  def update(input: Input, motor: Input): Unit = {
    sensoryInput.update(input)
    motorInput.update(motor)
    l4Layer.update()
    l5Layer.update()
    l6Layer.update()
    l3Layer.update()
  }

  def update(input: Input): Unit =
    update(input, IndexedSeq.fill(inputWidth)(false))
}

class L4Region[L](implicit val config: CLA.Config[L]) extends Region { region =>
  import CLA._
  import config._

  val numl4cells = 8

  val sensoryInput = new InputSDR[L]
  val motorInput = new InputSDR[L]
  val l4Input = new InputSDR[L]()(config.copy(inputWidth = config.numColumns * numl4cells))

  //TODO: increased segmentThreshold for l4 because of extra motor input?
  //TODO: zip input motor into same sdr? zip inputs?
  object l4Layer extends ExternalLearningLayer[L] {
    implicit val config = region.config.copy(columnHeight = numl4cells/*, burstCellDuration = 3, learningCellDuration = 1*/, maxDistalDendrites = maxDistalDendrites / 4)

    val id = "l4"

    val columns: IndexedSeq[Column[L]] =
      (0 until sensoryInput.segments.length).map { idx =>
        val segment = sensoryInput.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](this, loc, segment)
      }.toIndexedSeq

    def extraLayerLearningNodes: Stream[NeuralNode] = {
      region.motorInput.segments.toStream.filter(_.wasActive).sortBy(_.activationOrdinal).reverse
    }

    def update(): Unit = {
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }
  }

  object l3Layer extends InternalLearningLayer[L] {
    implicit val config = region.config.copy()

    val id = "l3"

    val columns: IndexedSeq[Column[L]] =
      (0 until l4Input.segments.length).map { idx =>
        val segment = l4Input.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new Column[L](this, loc, segment)
      }.toIndexedSeq

    def update(): Unit = {
      columns.foreach(_.update())
      columns.foreach(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
  }

  def update(input: Input, motor: Input): Unit = {
    sensoryInput.update(input) //spatial pooling
    motorInput.update(motor) //spatial pooling
    l4Layer.update()
    //if(math.random < 0.01) println(l4Layer.getInput.toSeq)
    l4Input.update(l4Layer)
    l3Layer.update()
  }

  def update(input: Input): Unit =
    update(input, IndexedSeq.empty)
}

//used to track 'active state'
trait ColumnState {
  type IndexAndUpdate = {
    def apply(index: Int): Boolean
    def update(index: Int, value: Boolean): Unit
  }

  object cellActive {
    def apply(index: Int): Boolean = ???
    def update(index: Int, value: Boolean): Unit = ???
  }

  object cellPredictive {
    def apply(index: Int): Boolean = ???
    def update(index: Int, value: Boolean): Unit = ???
  }
}

trait RegionState {
  val l3ColumnState: ColumnState
}

//holds the 'connectivity' / logic. the results of learning.
trait LearningState {

}