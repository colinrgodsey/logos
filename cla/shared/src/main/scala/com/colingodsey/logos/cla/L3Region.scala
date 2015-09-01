package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.ExtraMath.randomNormal
import com.colingodsey.logos.collections.RollingAverage

import scala.concurrent.ExecutionContext

trait Region {
  //protected type LocalL3Layer <: L3Layer
}

class L3Region[L](implicit val config: CLA.Config[L],
    val ec: ExecutionContext = CLA.VM.newDefaultExecutionContext) extends Region { region =>
  import CLA._
  import config._

  val inputLayer = new InputSDR[L]

  object l3Layer extends L3Layer[L] { l3Layer =>
    implicit val config = region.config

    val id = "l3"

    val columns: IndexedSeq[L3Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new L3Column[L](l3Layer, loc, segment)
      }.toIndexedSeq

    def update(): Unit = {
      columns.foreach(_.update())
      VM.distributedExec(desiredLocalActivity / numWorkers,
        activeColumns.toIndexedSeq)(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
    def maxDutyCycle: Double = region.maxDutyCycle
    def inhibitionRadius = inputLayer.inhibitionRadius
  }

  def maxDutyCycle = inputLayer.maxDutyCycle

  def update(input: Input): Unit = {
    inputLayer.update(input) //spatial pooling
    l3Layer.update()


  }
}

class L4Region[L](implicit val config: CLA.Config[L],
    val ec: ExecutionContext = CLA.VM.newDefaultExecutionContext) extends Region { region =>
  import CLA._
  import config._

  val numl4cells = 8

  val inputLayer = new InputSDR[L]
  val motorInput = new InputSDR[L]
  val l4Input = new InputSDR[L]()(config.copy(inputWidth = config.numColumns * numl4cells), ec)

  //TODO: increased segmentThreshold for l4 because of extra motor input?
  //TODO: zip input motor into same sdr? zip inputs?
  object l4Layer extends L4Layer[L] {
    implicit val config = region.config.copy(columnHeight = numl4cells/*, burstCellDuration = 3, learningCellDuration = 1*/, maxDistalDendrites = maxDistalDendrites / 4)

    val id = "l4"

    val columns: IndexedSeq[L4Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new L4Column[L](this, loc, segment)
      }.toIndexedSeq

    def maxDutyCycle = inputLayer.maxDutyCycle
    def inhibitionRadius = inputLayer.inhibitionRadius

    def motorInput: InputSDR[L] = region.motorInput

    def update(): Unit = {
      /*columns.foreach(_.preUpdate())
      columns.foreach(_.postUpdate())
      inhibitColumns()*/

      columns.foreach(_.update())
      VM.distributedExec(desiredLocalActivity / numWorkers,
        columns.toIndexedSeq)(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }
  }

  object l3Layer extends L3Layer[L] {
    implicit val config = region.config.copy()

    val id = "l3"

    val columns: IndexedSeq[L3Column[L]] =
      (0 until l4Input.segments.length).map { idx =>
        val segment = l4Input.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new L3Column[L](this, loc, segment)
      }.toIndexedSeq

    def update(): Unit = {
      columns.foreach(_.update())
      VM.distributedExec(desiredLocalActivity / numWorkers,
        columns.toIndexedSeq)(_.temporalPrePooler())
      columns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
    def maxDutyCycle = l4Input.maxDutyCycle
    def inhibitionRadius = l4Input.inhibitionRadius
  }

  def update(input: Input, motor: Input): Unit = {
    inputLayer.update(input) //spatial pooling
    motorInput.update(motor) //spatial pooling
    l4Layer.update()
    //if(math.random < 0.01) println(l4Layer.getInput.toSeq)
    l4Input.update(l4Layer)
    l3Layer.update()
  }

  def update(input: Input): Unit =
    update(input, Nil)
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