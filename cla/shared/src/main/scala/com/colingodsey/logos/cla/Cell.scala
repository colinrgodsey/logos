package com.colingodsey.logos.cla

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
  def predication =
    distalDendrite.mostActive.map(
      s => (s.activation, s.potentialActivation, math.random)) getOrElse (0, 0, 0.0)

  def seedDistal(n: Int): Unit = {
    val segments = distalDendrite.segments

    def randomSegment = segments((math.random * segments.length).toInt)

    //TODO: only find semi active columns?
    for {
      i <- 0 until n
      segment0 = distalDendrite.mostActive getOrElse randomSegment
      segment = if(segment0.synapses.length >= seededDistalConnections) randomSegment else segment0
      otherCell = region.getRandomCell(column, useLearnCell = true)
    } segment.synapses :+= otherCell -> region.getRandomDistalPermanence
  }

  def reinforceDistal(): Unit = {
    //distalDendrite.reinforce()

    var pruned = distalDendrite.reinforceAndPrune()

    distalDendrite.mostActive.foreach { segment =>
      if(segment.synapses.length < seededDistalConnections) {
        val otherCell = region.getRandomCell(column, useLearnCell = true)
        segment.synapses :+= otherCell -> region.getRandomDistalPermanence
        pruned -= 1
      }

    }

    if(pruned > 0) seedDistal(pruned)
  }

  def activateIfPredicted(): Unit = {
    if (predictive) activate() else deactivate()
  }
}
