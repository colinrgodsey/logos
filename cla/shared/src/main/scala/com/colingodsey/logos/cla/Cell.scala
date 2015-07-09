package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
final class Cell(val column: LearningColumn) extends NeuralNode { cell =>
  import column.layer
  import layer.config
  import config._

  type Location = layer.Location

  var predictive = false
  private var _active = false
  var activeForTicks = 0

  val distalDendrite = new DistalDendrite[Location](column.loc)
  var ordinal = math.random

  def active = _active

  //TODO: verticle offsets?
  def loc = column.loc

  def computePredictive(): Unit = {
    distalDendrite.update()

    predictive = distalDendrite.active
  }

  def leastPredictiveDutyCycle = distalDendrite.leastActiveDutyCycle
  def mostPredictiveDutyCycle = distalDendrite.mostActiveDutyCycle

  def tickDown(): Unit = {
    activeForTicks -= 1

    ordinal = math.random

    if(activeForTicks <= 0) deactivate()
  }

  protected def deactivate(): Unit = {
    _active = false
    //predictive = false
  }

  def activate(forTicks: Int): Unit = {
    _active = true
    //predictive = false
    //TODO: reset, or min?
    activeForTicks = math.min(forTicks, activeForTicks)
  }

  //TODO: count receptive, or no?
  //def predication = distalDendrite.mostActive.map(s => s.activation) getOrElse 0
  def activationOrdinal =
    distalDendrite.mostActive.map(_.activationOrdinal) getOrElse (0.0, 0.0, 0.0, ordinal)

  def randomSegment = distalDendrite.segments((math.random * distalDendrite.segments.length).toInt)

  def fillSegment(segment: DendriteSegment, learningCells: Stream[NeuralNode]): Unit = {
    if(learningCells.nonEmpty && segment.numConnections < config.seededDistalConnections)  {
      val cell = learningCells.head
      segment.addConnection(cell, getRandomDistalPermanence)
      fillSegment(segment, learningCells.tail)
    }
  }

  def addNewSegment(): Unit = {
    val segment = new DendriteSegment(distalDendrite)

    val learningCells = column.layer.getLearningNodes(cell.column)

    if(learningCells.length < segmentThreshold) return

    distalDendrite.segments :+= segment

    fillSegment(segment, learningCells)

    segment.update()
    segment.updateDutyCycle(force = true)
  }

  def reinforceDistal(): Unit = {
    distalDendrite.reinforce()

    val full = distalDendrite.isFull

    //TODO: reinforce the next active one if this is full?
    if(!distalDendrite.active && !full) {
      distalDendrite.mostActive match {
        /*case Some(segment) if segment.numConnections < config.seededDistalConnections =>
          fillSegment(segment, column.layer.getLearningNodes(column))*/
        case _ =>
          addNewSegment()
      }
    //TODO: maybe remove dendrites by activation amount, not activity
    } else if(full && distalDendrite.active) {
      val toRemove = distalDendrite.leastActiveDuty

      toRemove.foreach(distalDendrite.removeSegment(_))
    }
  }

  def activateIfPredicted(): Unit = {
    if (predictive) activate(learningCellDuration)
  }
}
