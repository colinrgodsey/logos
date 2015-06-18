package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext

trait MiniColumn extends NeuralNode { column =>
  var wasPredicted: Boolean
  var wasActive: Boolean
  var active: Boolean

  //def input: NeuralNode
  val layer: Layer

  def update(): Unit

  def ordinal: Double
}

trait LearningColumn extends MiniColumn {
  //def receptiveFieldRadius: Double
  val layer: SequenceLayer

  def loc: layer.Location
}

final class L4Column[L](val layer: L4Layer[L], val loc: L,
    val inputSegment: DendriteSegment) extends LearningColumn { column =>
  import layer.config
  import config._

  //val cell = new Cell(column)
  val ordinal = math.random

  var wasActive: Boolean = false
  var wasPredicted: Boolean = false
  var active: Boolean = false

  def update(): Unit = {
    wasActive = active

  }

  def checkFillSegment(segment: DistalDendrite[L], learningCells: => Stream[Cell]): Unit = {
    val full = segment.isFull

    if(!segment.active && !full) {
      segment.mostActive match {
        case Some(segment) if segment.numConnections < config.seededDistalConnections =>
          fillSegment(segment, learningCells)
        case _ =>
          addNewSegment(segment, learningCells)
      }

    } else if(full && segment.active) {
      val toRemove = segment.leastActiveDuty

      toRemove.foreach(segment.removeSegment(_))
    }
  }

  protected def fillSegment(segment: DendriteSegment, learningCells: Stream[Cell]): Unit = {
    if(learningCells.nonEmpty && segment.numConnections < config.seededDistalConnections)  {
      val cell = learningCells.head
      segment.addConnection(cell, getRandomDistalPermanence)
      fillSegment(segment, learningCells.tail)
    }
  }

  protected def addNewSegment(dendrite: DistalDendrite[L], getLearningCells: => Stream[Cell]): Unit = {
    val segment = new DendriteSegment(dendrite)

    val learningCells = getLearningCells

    if(learningCells.isEmpty) return

    dendrite.segments :+= segment

    fillSegment(segment, learningCells)

    segment.update()
    segment.updateDutyCycle(force = true)
  }
}

final class L3Column[L](val layer: L3Layer[L], val loc: L,
    val inputSegment: DendriteSegment) extends LearningColumn { column =>
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
  //def receptiveFieldRadius = inputSegment.receptiveRadius(loc)

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
