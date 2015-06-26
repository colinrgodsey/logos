package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext

trait MiniColumn extends NeuralNode { column =>
  var wasPredicted: Boolean
  var wasActive: Boolean
  def active: Boolean

  //def input: NeuralNode
  val layer: Layer

  def ordinal: Double
}

trait LearningColumn extends MiniColumn {
  //def receptiveFieldRadius: Double
  val layer: SequenceLayer

  def loc: layer.Location
}

final class L4Column[L](val layer: L4Layer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>
  import layer.config
  import config._

  //val learningCell = new Cell(column)
  var learnedTransitions = Set[(L4Column[L], DendriteSegment)]()

  val ordinal = math.random

  var wasActive: Boolean = false
  var feedForwardActive: Boolean = false
  var wasPredicted: Boolean = false
  var activeCount: Int = 0

  def active = activeCount > 0

  def boost = inputSegment match {
    case x: SDR => x.boost
    case _ => 0.0
  }
  def overlap = inputSegment match {
    case x: SDR => x.overlap
    case _ => 0.0
  }

  def learnTransition() = {
    val learningColumn = layer.getLearningColumn
    val learningMotor = layer.getLearningMotorNodes.take(seededDistalConnections)

    if(learningColumn.active && learningMotor.length > minOverlap) {

      val segment = new DendriteSegment(layer, activationThresholdOpt = Some(minOverlap))
      learningMotor foreach { s =>
        segment.addConnection(s, connectionThreshold / 2.0)
      }
println("learned l4 transition")
      learnedTransitions += learningColumn -> segment
    }
  }

  def preUpdate(): Unit = {
    wasActive = active

    feedForwardActive = inputSegment.active

    if(activeCount > 0) activeCount -= 1
    if(activeCount > 30) activeCount = 30

    if(feedForwardActive && !active) activeCount += 1//4

    if(wasPredicted) activeCount -= 3

    if(activeCount < 0) activeCount = 0
  }

  def postUpdate(): Unit = {
    learnedTransitions.foreach(_._2.update())
    learnedTransitions.foreach {
      case (_, segment) =>
        segment.update()
        segment.updateDutyCycle()
    }

    wasPredicted = learnedTransitions.exists {
      case (c, s) =>
        val res = c.active && s.active

        c.activeCount += 10

        res
    }

    if(!wasPredicted) {
      learnTransition()

      if(learnedTransitions.size > maxDistalDendrites) {
        val p = learnedTransitions.minBy(_._2.activeDutyCycle.toDouble)

        learnedTransitions -= p
      }
    }

    if(wasPredicted) {
      val max = learnedTransitions.maxBy(pair => (pair._1.active, pair._2.activationOrdinal))._2

      if(max.active) max.reinforce()
    }

  }
}

final class L3Column[L](val layer: L3Layer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>
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

  def boost = inputSegment match {
    case x: SDR => x.boost
    case _ => 0.0
  }
  def overlap = inputSegment match {
    case x: SDR => x.overlap
    case _ => 0.0
  }

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
  def temporalPostPooler(): Unit = {
    cells.foreach(_.tickDown())

    if(active) cells.foreach(_.activateIfPredicted())

    //TODO: wasPredicted only if active this round?
    wasPredicted = cells.exists(_.active)

    //burst cells if not predicted
    if (!wasPredicted && active)
      cells.foreach(_.activate(1))
  }

  def update(): Unit = {
    wasActive = active
    active = inputSegment.active
  }
}
