package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.ExtraMath.randomNormal
import com.colingodsey.logos.collections.RollingAverage

import scala.concurrent.ExecutionContext

trait Layer extends DutyCycle.Booster {
  implicit val config: CLA.Config
}

trait SequenceLayer extends Layer {
  import CLA._

  type ColumnType <: Column

  def columns: IndexedSeq[ColumnType]
  def getLearningCells: Stream[Cell]
  //def input(idx: Int): Boolean
  def inhibitionRadius: Double

  def columnsNear(loc: Location, rad: Radius) = {
    val min = math.max(0, loc - rad).toInt
    val max = math.min(config.regionWidth - 1, loc + rad).toInt

    (min to max).iterator map columns
  }

  def numActiveFromPrediction = columns.count(column => column.active && column.wasPredicted)
  def numActive = columns.count(_.active)

  def anomalyScore = {
    val na = numActive.toDouble

    if(na == 0) 0.0
    else 1.0 - numActiveFromPrediction / na
  }

  def averageReceptiveFieldRadius = {
    //TODO: filter or not?
    val filtered = columns//.filter(_.active)

    val sum = filtered.map(_.receptiveFieldRadius).sum

    sum.toDouble / filtered.length
  }

  def getLearningCells(c: Column): Stream[Cell]  =
    getLearningCells.filter(_.column != c)

  def activeColumns = columns.iterator.filter(_.active)
}

trait L3Layer extends SequenceLayer {
  override type ColumnType = L3Column

  def getLearningCells: Stream[Cell] = {
    columns.toStream.sortBy { c =>
      (!c.wasPredicted, !c.wasActive, c.ordinal)
    }.map(_.learningCell)
  }
}

class L3Region(implicit val config: CLA.Config,
    val ec: ExecutionContext = CLA.VM.newDefaultExecutionContext) extends L3Layer { region =>
  import CLA._
  import config._

  val inputLayer = new InputSDR

  val l3Layer = new L3Layer {
    def update(): Unit = {
      columns.foreach(_.update())
      VM.distributedExec(desiredLocalActivity / numWorkers,
        activeColumns.toIndexedSeq)(_.temporalPrePooler())
      activeColumns.foreach(_.temporalPostPooler())
    }

    def columns: IndexedSeq[ColumnType] = region.columns

    //def input(idx: Int): Boolean
    def inhibitionRadius: Radius = region.inhibitionRadiusAverage.toDouble

    implicit val config: Config = region.config

    def maxDutyCycle: Radius = region.maxDutyCycle
  }

  val columns: IndexedSeq[L3Column] = inputLayer.segments.map { segment =>
    new L3Column(region, segment.loc, segment)
  }

  var inhibitionRadiusAverage = RollingAverage(dutyAverageFrames)
  def maxDutyCycle = inputLayer.maxDutyCycle

  inhibitionRadiusAverage += regionWidth / 2.0

  def inhibitionRadius: Radius = math.max(inhibitionRadiusAverage.toDouble, 3)

  def update(input: Input): Unit = {
    inputLayer.update(input) //spatial pooling
    l3Layer.update()

    inhibitionRadiusAverage += inhibitionRadius
  }

}

