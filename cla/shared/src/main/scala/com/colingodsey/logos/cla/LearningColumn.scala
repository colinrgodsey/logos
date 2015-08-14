package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext
import scala.util.Try

trait MiniColumn extends NeuralNode { column =>
  var wasPredicted: Boolean
  var wasActive: Boolean
  def active: Boolean

  //def input: NeuralNode
  val layer: Layer

  def ordinal: Double
}

trait LearningColumn extends MiniColumn { column =>

  val layer: SequenceLayer

  def loc: layer.Location

  import layer.config
  import config._

  val cells = Array.fill(columnHeight)(new Cell(column))
  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random)
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random)

  val uuid = math.random

  var active = false
  @volatile private var selectedLearningCell: Option[Cell] = None
  @volatile private var nextLearningCell: Option[Cell] = None
  var wasPredicted = false
  var wasActive = false

  def randomCell = cells((cells.length * math.random).toInt)
  def learningCell = selectedLearningCell.getOrElse(randomCell)
  def activeCells = cells.filter(_.active)

  def cellIndexes = 0 until columnHeight

  def ordinal = uuid

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

  /**
   * Calculate predictive state here.
   * No modification should be made to any cells or columns here!
   * We should just predict what happens in the post pooler.
   */
  def temporalPrePooler(): Unit = if(active) {
    cells.foreach(_.computePredictive())

    val hasPredictive = cells.exists(_.predictive)

    /*
    STICKY CELL HYPOTHESIS
    is it best to stick with one learning cell per column until
    it becomes relevent. This is because the learning cell is selected
    to learn prediction patterns as well as selected for predicting
    the next pattern. Therefor it is important this cell becomes
    relevent at some point.
     */

    //only select new learning cell once old one is predicted or new one predicted
    nextLearningCell = if(learningCell.predictive || hasPredictive) {
      val max = cells.maxBy(_.activationOrdinal)

      Some(max)
    } else Some(learningCell)

    /*
    Calculate any distal mutations here. These calculations
    should NOT affect any of the data used by other columns in
    the pre pooler.
     */
    if(hasPredictive) {
      //cells.filter(_.predictive).foreach(_.reinforceDistal())
      nextLearningCell.getOrElse(randomCell).reinforceDistal()
    } else {
      //only reinforce the 'learning cell' here (max predication)
      nextLearningCell.getOrElse(randomCell).reinforceDistal()
    }
  }

  /**
   * This handles actually mutating cell/column state
   * based on what was predicted in the pre pooler.
   */
  def temporalPostPooler(): Unit = {
    cells.foreach(_.tickDown())

    //TODO: only 1 cell? or many?
    wasPredicted = if(active) {
      //val nPredictive = cells.count(_.activateIfPredicted())
      val nPredictive = cells.count(_.predictive)
      //if(nextLearningCell.isDefined && nextLearningCell.get.predictive) learningCell.activate(learningCellDuration)

      //TODO: wasPredicted only if active this round?
      nPredictive > 0
    } else false

    //burst cells if not predicted
    if (!wasPredicted && active)
      cells.foreach(_.activate(burstCellDuration))

    if(active) {
      selectedLearningCell = nextLearningCell
      //selectedLearningCell.foreach(_.activateIfPredicted())
      selectedLearningCell.foreach(_.activate(learningCellDuration))
    }
  }
}

final class L4Column[L](val layer: L4Layer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>

  var oldOverlap = 0.0

  def boost = inputSegment match {
    case x: SDR => x.boost
    case _ => 0.0
  }
  def overlap = inputSegment match {
    case x: SDR => x.overlap
    case _ => 0.0
  }

  def update(): Unit = {
    wasActive = active
    oldOverlap = overlap
    active = inputSegment.active
  }
}

final class L3Column[L](val layer: L3Layer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>

  var oldOverlap = 0.0

  def boost = inputSegment match {
    case x: SDR => x.boost
    case _ => 0.0
  }
  def overlap = inputSegment match {
    case x: SDR => x.overlap
    case _ => 0.0
  }

  def update(): Unit = {
    wasActive = active
    oldOverlap = overlap
    active = inputSegment.active

    //TODO: errr
    //Try(randomCell.randomSegment.reinforce())
  }
}

/*
//TODO: this should maybe be done with an SDR on the columns, and not just a single column
final class L4Column[L](val layer: L4Layer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>
  import layer.config
  import config._

  //val learningCell = new Cell(column)
  var learnedTransitions = Set[(DendriteSegment, DendriteSegment)]()

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
    val learningColumns = layer.getLearningColumns.take(seededDistalConnections)
    val learningMotor = layer.getLearningMotorNodes.take(seededDistalConnections)

    if(learningMotor.nonEmpty && learningMotor.length >= minOverlap && learningColumns.length >= minOverlap) {
      val motorSegment = new DendriteSegment(layer, activationThresholdOpt = Some(minOverlap))
      learningMotor foreach { s =>
        motorSegment.addConnection(s, getRandomDistalPermanence)
      }
println("new l4 trans!")
      val inputSegment = new DendriteSegment(layer, activationThresholdOpt = Some(minOverlap))
      learningColumns foreach { c =>
        inputSegment.addConnection(new topology.LocalNeuralNode {
          def loc = c.loc
          def active: Boolean = c.feedForwardActive
        }, getRandomDistalPermanence)
      }

      learnedTransitions += inputSegment -> motorSegment
    }
  }

  //update transitions first before changing column activation
  def preUpdate(): Unit = {
    if(inputSegment.active) {
      learnedTransitions.foreach {
        case (cSegment, mSegment) =>
          mSegment.update()
          mSegment.updateDutyCycle()
          cSegment.update()
          cSegment.updateDutyCycle()
      }

      wasPredicted = learnedTransitions.exists {
        case (c, s) => c.active && s.active
      }

      if(!wasPredicted && math.random < 0.2) {
        if(learnedTransitions.size > maxDistalDendrites) {
          //val p = learnedTransitions.minBy(_._2.activeDutyCycle.toDouble)
          val p = learnedTransitions.minBy {
            case (iSegment, mSegment) =>
              iSegment.activeDutyCycle.toDouble + mSegment.activeDutyCycle.toDouble
          }

          learnedTransitions -= p
        }

        learnTransition()
      }

      if(wasPredicted) {
        //val max = learnedTransitions.maxBy(pair => (pair._1.active, pair._2.activationOrdinal))._2
        val max = learnedTransitions.maxBy {
          case (iSegment, mSegment) =>
            val a = iSegment.activationOrdinal
            val b = mSegment.activationOrdinal

            val activationOrdinal = (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4)
            (iSegment.active, mSegment.active, activationOrdinal)
        }

        max._1.reinforce()
        max._2.reinforce()
      }
    }
  }

  def postUpdate(): Unit = {
    wasActive = active

    feedForwardActive = inputSegment.active

    if(activeCount > 0) activeCount -= 1
    if(activeCount > 30) activeCount = 30

    if(feedForwardActive) {
      if(!active) activeCount += 3

      activeCount += 1
    }

    if(wasPredicted) activeCount = 0
    if(activeCount < 0) activeCount = 0
  }
}*/
