package com.colingodsey.logos.cla.traits

import com.colingodsey.logos.cla.NeuralNode
import com.colingodsey.logos.collections.RollingAverage

trait DutyCycle extends NeuralNode {
  private var _boost = 0.0
  protected val permanentBoostOffset = math.random * 0.1

  def boostIncr: Double
  def activationThreshold: Int
  def activation: Int
  def active: Boolean
  def boostPermanence(): Unit
  def dutyCycleUpdateRatio: Double
  val activeDutyCycle: RollingAverage
  val overlapDutyCycle: RollingAverage

  def minThreshold: Double = activationThreshold

  def competition = math.max(overlapDutyCycle.toDouble - activeDutyCycle.toDouble, 0.0)

  def boost = _boost
  protected def boost_=(x: Double) = _boost = x

  def overlap: Double = {
    val a = activation

    if(a < minThreshold) 0.0
    else a * (1.0 + boost) + permanentBoostOffset
  }

  def activeOverlap = if(active) overlap else 0.0

  //TODO: data from parent, or inhibition radius?
  def updateDutyCycle(maxDutyCycle: Double, force: Boolean = false): Unit =
    if(math.random < dutyCycleUpdateRatio || force) {
      val minDutyCycle = 0.01 * maxDutyCycle

      activeDutyCycle += (if(active) 1 else 0)
      overlapDutyCycle += (if(overlap >= activationThreshold) 1 else 0)

      if(activeDutyCycle.toDouble < minDutyCycle) boost += boostIncr * math.random
      else boost = 0

      //enforce all synapses a small amount
      if(overlapDutyCycle.toDouble <= minDutyCycle)
        boostPermanence()
    }

}