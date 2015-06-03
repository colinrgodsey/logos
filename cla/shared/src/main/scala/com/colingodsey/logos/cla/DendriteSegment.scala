package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.DutyCycle.Booster
import com.colingodsey.logos.collections.RollingAverage

//TODO: calculate if this has ever fired, drop if not
final class DendriteSegment(
    val loc: CLA.Location,
    synapses: IndexedSeq[(NeuralNode, Double)] = IndexedSeq.empty)(
    implicit val config: CLA.Config) extends SDR {
  import config._

  var active = false
  var activation = 0
  var potentialActivation = 0
  var receptive = 0

  var activeDutyCycle = RollingAverage(dutyAverageFrames)
  var overlapDutyCycle = RollingAverage(dutyAverageFrames)
  //var sequenceSegment = false

  var connections = synapses

  def receptiveRadius = {
    connections.iterator.map {
      case (node, p) if p > connectionThreshold =>
        math.abs(node.loc - loc)
      case _ => 0
    }.max
  }

  //TODO: min activation for boost?
  def activationOrdinal = (activation * (boost + 1.0), potentialActivation, math.random)

  def update(): Unit = {
    var act = 0
    var rec = 0
    var potAct = 0
    var i = 0

    val l = connections.length

    while(i < l) {
      val pair = connections(i)
      val node = pair._1
      val p = pair._2

      val con = p > connectionThreshold

      if(con) rec += 1
      else if(node.active) potAct += 1

      if(con && node.active) act += 1

      i += 1
    }

    activation = act
    receptive = rec
    potentialActivation = potAct

    active = activation > activationThreshold

    //updateDutyCycle()
  }

  def connectionThreshold: Double = config.connectionThreshold
  def minDistalPermanence: Double = config.minDistalPermanence
  def permanenceInc: Double = config.permanenceInc
  def permanenceDec: Double = config.permanenceDec
  def boostIncr: Double = config.boostIncr
  def activationThreshold: Int = config.segmentThreshold

  def parent: Booster = new Booster {
    def maxDutyCycle: Double =  0.0
  }
}
