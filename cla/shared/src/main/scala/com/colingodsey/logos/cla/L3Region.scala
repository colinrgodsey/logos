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

  def maxDutyCycle = inputLayer.maxDutyCycle

  object l3Layer extends L3Layer[L] { l3Layer =>
    implicit val config = region.config

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
      activeColumns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
    def maxDutyCycle: Double = region.maxDutyCycle
    def inhibitionRadius = inputLayer.inhibitionRadius
  }



  def update(input: Input): Unit = {
    inputLayer.update(input) //spatial pooling
    l3Layer.update()


  }
}

class L4Region[L](implicit val config: CLA.Config[L],
    val ec: ExecutionContext = CLA.VM.newDefaultExecutionContext) extends Region { region =>
  import CLA._
  import config._

  val inputLayer = new InputSDR[L]
  val motorInput = new InputSDR[L]

  def maxDutyCycle = inputLayer.maxDutyCycle

  def update(input: Input, motor: Input): Unit = {
    inputLayer.update(input) //spatial pooling
    motorInput.update(motor) //spatial pooling
    l4Layer.update()
    l3Layer.update()
  }

  object l4Layer extends L4Layer[L] {
    implicit val config = region.config

    val columns: IndexedSeq[L4Column[L]] =
      (0 until inputLayer.segments.length).map { idx =>
        val segment = inputLayer.segments(idx)
        val loc = topology.columnLocationFor(idx)

        new L4Column[L](this, loc, segment)
      }.toIndexedSeq

    def maxDutyCycle: Double = region.maxDutyCycle
    def inhibitionRadius = inputLayer.inhibitionRadius

    def motorInput: InputSDR[L] = region.motorInput

    def update(): Unit = {
      columns.foreach(_.preUpdate())
      columns.foreach(_.postUpdate())
    }
  }

  object l3Layer extends L3Layer[L] {
    implicit val config = region.config

    val columns: IndexedSeq[L3Column[L]] =
      l4Layer.columns.map { column =>
        new L3Column[L](this, column.loc, column)
      }.toIndexedSeq

    def update(): Unit = {
      columns.foreach(_.update())
      VM.distributedExec(desiredLocalActivity / numWorkers,
        activeColumns.toIndexedSeq)(_.temporalPrePooler())
      activeColumns.foreach(_.temporalPostPooler())
    }

    //def input(idx: Int): Boolean
    def maxDutyCycle: Double = region.maxDutyCycle
    def inhibitionRadius = inputLayer.inhibitionRadius
  }
}