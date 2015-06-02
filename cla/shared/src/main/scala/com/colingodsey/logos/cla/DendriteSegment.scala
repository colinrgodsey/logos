package com.colingodsey.logos.cla

final class DendriteSegment(
    val loc: CLA.Location,
    var synapses: IndexedSeq[(NeuralNode, Double)] = IndexedSeq.empty)(
    implicit val config: CLA.Config) extends NeuralNode {
  import config._

  var active = false
  var activation = 0
  var receptive = 0
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

  def update(): Unit = {
    var act = 0
    var rec = 0
    var i = 0

    val l = synapses.length

    while(i < l) {
      val pair = synapses(i)
      val node = pair._1
      val p = pair._2

      val r = p > connectionThreshold

      if(r) rec += 1

      if(r && node.active) act += 1

      i += 1
    }

    activation = act
    receptive = rec

    active = activation > threshold
  }
}
