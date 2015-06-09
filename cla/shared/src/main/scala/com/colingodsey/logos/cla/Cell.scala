package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
final class Cell(val column: Column) extends NeuralNode { cell =>
  import column.region
  import region.config
  import config._

  var predictive = false
  private var _active = false

  val distalDendrite = new DistalDendrite(column.loc)
  val ordinal = math.random

  def active = _active

  //TODO: verticle offsets?
  def loc = column.loc

  def computePredictive(): Unit = {
    distalDendrite.update()

    predictive = distalDendrite.active
  }

  def leastPredictiveDutyCycle = distalDendrite.leastActiveDuty.activeDutyCycle
  def mostPredictiveDutyCycle = distalDendrite.mostActiveDuty.activeDutyCycle

  def deactivate(): Unit = {
    _active = false
    predictive = false
  }

  def activate(): Unit = {
    _active = true
    predictive = false
  }

  //TODO: count receptive, or no?
  //def predication = distalDendrite.mostActive.map(s => s.activation) getOrElse 0
  def activationOrdinal =
    distalDendrite.mostActive.map(_.activationOrdinal) getOrElse (0.0, 0.0, 0.0, 0.0)

  def randomSegment = distalDendrite.segments((math.random * distalDendrite.segments.length).toInt)

  def seedDistal(n: Int): Unit = {
    //TODO: only find semi active columns?
    for {
      i <- 0 until n
      segment0 = randomSegment
      segment = if(segment0.numConnections >= seededDistalConnections) randomSegment else segment0
      otherCell = region.getRandomCell(column, useLearnCell = true)
    } segment.addConnection(otherCell, region.getRandomDistalPermanence)
  }

  def addNewSegment(): Unit = {
    val segment = new DendriteSegment(loc, distalDendrite)

    val predictedColumns = column.region.columns.filter(c => c.wasActive && c != cell.column)

    if(predictedColumns.isEmpty) return

    distalDendrite.segments :+= segment

    for(_ <- 0 until config.seededDistalConnections) {
      val selected = predictedColumns((predictedColumns.length * math.random).toInt)

      val cell = selected.learningCell

      segment.addConnection(cell, region.getRandomDistalPermanence)
    }
  }

  def reinforceDistal(): Unit = {
    //distalDendrite.reinforce()

    val pruned = distalDendrite.reinforceAndPrune()

    val full = distalDendrite.isFull

    if(!distalDendrite.active && !full) {
      addNewSegment()
    } else if(full) {
      val toRemove = distalDendrite.leastActiveDuty

      distalDendrite.segments =
        distalDendrite.segments.filter(_ != toRemove)
    }
  }

  def activateIfPredicted(): Unit = {
    if (predictive) activate() else deactivate()
  }
}
