package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.{TypedSequenceLayer, LearningColumn, SequenceLayer, SDR}
import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext
import scala.util.Try





final class Column[L](val layer: TypedSequenceLayer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>

  var oldOverlap = 0.0

  lazy val id = loc.toString

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
