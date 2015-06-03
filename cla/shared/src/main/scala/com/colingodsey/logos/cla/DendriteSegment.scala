package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

//TODO: calculate if this has ever fired, drop if not
final class DendriteSegment(
    val loc: CLA.Location,
    var synapses: IndexedSeq[(NeuralNode, Double)] = IndexedSeq.empty)(
    implicit val config: CLA.Config) extends NeuralNode {
  import config._

  var active = false
  var activation = 0
  var potentialActivation = 0
  var receptive = 0
  var boost = 0.0

  var activeDutyCycle = RollingAverage(dutyAverageFrames)
  //var sequenceSegment = false

  def threshold = segmentThreshold

  def receptiveRadius = {
    synapses.iterator.map {
      case (node, p) if p > connectionThreshold =>
        math.abs(node.loc - loc)
      case _ => 0
    }.max
  }

  //TODO: min activation?
  def reinforce(): Unit = /*if(activation > minActivation)*/ {
    synapses = synapses map {
      case (node, p) =>
        val newP =
          if (node.active) math.min(1.0, p + permanenceInc)
          else math.max(0.0, p - permanenceDec)

        node -> newP
    }
  }

  //TODO: min activation for boost?
  def activationOrdinal = (activation * (boost + 1.0), potentialActivation, math.random)

  def pruneSynapses(): Int = {
    var pruned = 0

    synapses = synapses filter {
      case (_, p) if p < minDistalPermanence =>
        pruned += 1
        false
      case _ => true
    }

    pruned
  }

  def addBoost(): Unit = {
    boost += boostIncr
  }

  def update(): Unit = {
    var act = 0
    var rec = 0
    var potAct = 0
    var i = 0

    val l = synapses.length

    while(i < l) {
      val pair = synapses(i)
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

    active = activation > threshold

    activeDutyCycle += (if(active) 1.0 else 0.0)

    if(active) boost = 0.0
  }
}
