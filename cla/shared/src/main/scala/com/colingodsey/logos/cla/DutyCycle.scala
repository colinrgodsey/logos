package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

object DutyCycle {
  trait Booster {
    def maxDutyCycle: Double
  }

  trait Inhibitor {

  }
}

trait DutyCycle extends NeuralNode {
  private var _boost = 0.0

  def parent: DutyCycle.Booster

  def boostIncr: Double
  def activationThreshold: Int
  def activation: Int
  def active: Boolean
  def boostPermanence(): Unit
  def dutyCycleUpdateRatio: Double
  val activeDutyCycle: RollingAverage
  val overlapDutyCycle: RollingAverage

  def boost = _boost
  protected def boost_=(x: Double) = _boost = x

  def overlap: Double = {
    val a = activation

    if(a < activationThreshold) 0.0
    else a * (1.0 + boost)
  }

  //TODO: data from parent, or inhibition radius?
  def updateDutyCycle(): Unit = if(math.random < dutyCycleUpdateRatio) {
    //val maxDutyCycle = neighborsIn(region.inhibitionRadius).map(_.activeDutyCycle.toDouble).max

    val maxDutyCycle = parent.maxDutyCycle
    val minDutyCycle = 0.01 * maxDutyCycle

    activeDutyCycle += (if(active) 1 else 0)
    overlapDutyCycle += (if(overlap >= activationThreshold) 1 else 0)

    if(activeDutyCycle.toDouble < minDutyCycle) boost += boostIncr
    else boost = 0

    //enforce all synapses a small amount
    if(overlapDutyCycle.toDouble <= minDutyCycle)
      boostPermanence()
  }

}