package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
final class Cell(val column: Column) extends NeuralNode {
  import column.region
  import region.config
  import config._

  var predictive = false
  private var _active = false

  val distalDendrite = new DistalDendrite(column.loc)

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
    distalDendrite.mostActive.map(_.activationOrdinal) getOrElse (0.0, 0, 0.0)

  def randomSegment = distalDendrite.segments((math.random * distalDendrite.segments.length).toInt)

  def seedDistal(n: Int): Unit = {
    //TODO: only find semi active columns?
    for {
      i <- 0 until n
      segment0 = randomSegment
      segment = if(segment0.connections.length >= seededDistalConnections) randomSegment else segment0
      otherCell = region.getRandomCell(column, useLearnCell = true)
    } segment.connections :+= otherCell -> region.getRandomDistalPermanence
  }


  def reinforceDistal(): Unit = {
    //distalDendrite.reinforce()

    val pruned = distalDendrite.reinforceAndPrune()

    /*if(math.random < 0.2) {
      val segment = randomSegment

      segment.reinforce()
      pruned += segment.pruneSynapses()
    }*/

    if(distalDendrite.synapseFillPercent < 1 || pruned > 0) {
      if(pruned > 0) seedDistal(pruned)

      def otherCell = region.getRandomCell(column, useLearnCell = true)

      distalDendrite.mostActive match {
        case Some(segment) if segment.connections.length < seededDistalConnections =>
          segment.connections :+= otherCell -> region.getRandomDistalPermanence
          //seedDistal(math.max(1, pruned - 1))
        case _ if distalDendrite.synapseFillPercent < 0.5 =>
          seedDistal(5)
        case _ =>
          distalDendrite.leastActiveDuty.connections :+= otherCell -> region.getRandomDistalPermanence
          //seedDistal(math.max(1, pruned))
      }
    }
  }

  def activateIfPredicted(): Unit = {
    if (predictive) activate() else deactivate()
  }
}
