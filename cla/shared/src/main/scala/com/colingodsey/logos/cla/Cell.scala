package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
final class Cell(val column: Column) extends NeuralNode { cell =>
  import column.layer
  import layer.config
  import config._

  var predictive = false
  private var _active = false
  var activeForTicks = 0

  val distalDendrite = new DistalDendrite(column.loc)
  val ordinal = math.random

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

    if(activeForTicks <= 0) deactivate()
  }

  protected def deactivate(): Unit = {
    _active = false
    predictive = false
  }

  def activate(forTicks: Int): Unit = {
    _active = true
    predictive = false
    activeForTicks = forTicks
  }

  //TODO: count receptive, or no?
  //def predication = distalDendrite.mostActive.map(s => s.activation) getOrElse 0
  def activationOrdinal =
    distalDendrite.mostActive.map(_.activationOrdinal) getOrElse (0.0, 0.0, 0.0, math.random)

  def randomSegment = distalDendrite.segments((math.random * distalDendrite.segments.length).toInt)

  def fillSegment(segment: DendriteSegment, learningCells: Stream[Cell]): Unit = {
    if(learningCells.nonEmpty && segment.numConnections < config.seededDistalConnections)  {
      val cell = learningCells.head
      segment.addConnection(cell, getRandomDistalPermanence)
      fillSegment(segment, learningCells.tail)
    }
  }

  def addNewSegment(): Unit = {
    val segment = new DendriteSegment(loc, distalDendrite)

    val learningCells = column.layer.getLearningCells(cell.column)

    if(learningCells.isEmpty) return

    distalDendrite.segments :+= segment

    fillSegment(segment, learningCells)

    segment.update()
    segment.updateDutyCycle(force = true)
  }

  def reinforceDistal(): Unit = {
    //distalDendrite.reinforce()

    val pruned = distalDendrite.reinforceAndPrune()

    val full = distalDendrite.isFull

    //TODO: reinforce the next active one if this is full?
    if(!distalDendrite.active && !full) {
      distalDendrite.mostActive match {
        case Some(segment) if segment.numConnections < config.seededDistalConnections =>
          fillSegment(segment, column.layer.getLearningCells(column))
        case _ =>
          addNewSegment()
      }

    } else if(full && distalDendrite.active) {
      val toRemove = distalDendrite.leastActiveDuty

      toRemove.foreach(distalDendrite.removeSegment(_))
    }
  }

  def activateIfPredicted(): Unit = {
    if (predictive) activate(learningCellDuration) else tickDown()
  }
}
