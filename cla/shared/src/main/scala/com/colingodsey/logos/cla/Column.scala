package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext

trait MiniColumn extends NeuralNode { column =>
  var wasPredicted: Boolean
  var wasActive: Boolean
  var active: Boolean

  //def input: NeuralNode
  val layer: SequenceLayer

  def update(): Unit

  def ordinal: Double
}

trait Column extends MiniColumn {
  def receptiveFieldRadius: Double
}



final class L3Column(val layer: SequenceLayer, val loc: CLA.Location,
    val inputSegment: DendriteSegment) extends Column { column =>
  import layer.config
  import config._

  val cells = Array.fill(columnHeight)(new Cell(column))
  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random)
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random)

  val uuid = math.random

  var active = false
  @volatile var selectedLearningCell: Option[Cell] = None
  var wasPredicted = false
  var wasActive = false

  def randomCell = cells((cells.length * math.random).toInt)
  def learningCell = selectedLearningCell.getOrElse(randomCell)

  def cellIndexes = 0 until columnHeight

  def ordinal = uuid

  def boost = inputSegment.boost
  def overlap = inputSegment.overlap

  override def toString = {
    val active = cells.map {
      case cell if cell.active && wasPredicted => "P"
      case cell if cell.active => "A"
      case _ => "O"
    }.mkString

    s"$active loc: $loc "
  }

  //def receptiveFieldSize = proximalDendrite.receptive
  def receptiveFieldRadius = inputSegment.receptiveRadius

  def neighborsIn(radius: CLA.Radius) = ??? //layer.columnsNear(loc, radius).filter(_ != column)

  //only for strict inhibition radius
  /*def spatialPooler(): Unit = {
    val sorted = neighborsIn(layer.inhibitionRadius).toStream.
        map(_.overlap).filter(_ >= minOverlap).sortBy(-_)

    val min = if(sorted.isEmpty) minOverlap
    else sorted.take(desiredLocalActivity).min

    if(overlap >= min) activate()
  }*/

  def predication = learningCell.activationOrdinal._1

  def leastPredictiveDutyCell =
    cells.minBy(x => (x.leastPredictiveDutyCycle, x.ordinal))
  def mostPredictiveDutyCell =
    cells.maxBy(x => (x.mostPredictiveDutyCycle, x.ordinal))

  //active columns will 'tick' predictive state of cells
  //TODO: should this fire if active, or if overlap > minOverlap?
  def temporalPrePooler(): Unit = if(active) {
    cells.foreach(_.computePredictive())

    //TOD: this is breaking consistency. updates happening while reffing other selectedLearningCell
    selectedLearningCell = Some(cells.maxBy(_.activationOrdinal))

    val hasPredictive = cells.exists(_.predictive)

    //TODO: only learn one segment at a time?
    //TODO: only new synapses to learning cells?
    if(hasPredictive) {
      //TODO: most predictive only, or all predictive?
      //cells.filter(_.predictive).foreach(_.reinforceDistal())
      learningCell.reinforceDistal()
    } else {
      //only reinforce the 'learning cell' here (max predication)
      learningCell.reinforceDistal()
    }
  }

  //TODO: learning cell and sequence segments
  def temporalPostPooler(): Unit = if(active) {
    cells.foreach(_.activateIfPredicted())

    wasPredicted = cells.exists(_.active)

    /*
    if none active from prediction, activate all
    for our 'context-less' activation.
    Otherwise deactivate all.
    */
    if(!wasPredicted) {
      cells.foreach(_.activate(1))
    }
  } else {
    //deactivate all
    cells.foreach(_.tickDown())
    wasPredicted = cells.exists(_.active)
  }

  def update(): Unit = {
    wasActive = active
    active = inputSegment.active
  }
}
