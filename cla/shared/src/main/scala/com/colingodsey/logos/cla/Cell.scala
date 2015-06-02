package com.colingodsey.logos.cla

/**
 * Created by crgodsey on 6/2/15.
 */
//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
final class Cell(val column: Column) extends NeuralNode {
  import column.region

  var predictive = false
  private var _active = false

  val distalDendrite = new DistalDendrite

  def active = _active

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

  def seedDistal(n: Int): Unit = {
    val segments = distalDendrite.segments

    //TODO: only find semi active columns?
    for {
      i <- 0 until n
      segment = segments((math.random * segments.length).toInt)
      otherCell = region.getRandomCell(column)
    } segment.synapses += otherCell -> region.getRandomPermanence
  }

  def reinforceDistal(): Unit = {
    val pruned = distalDendrite.pruneSynapses()

    if(pruned > 0) seedDistal(pruned)

    distalDendrite.reinforce()
  }

  def activateIfPredicted(): Unit = {
    if (predictive) activate() else deactivate()
  }
}
