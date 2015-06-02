package com.colingodsey.logos.cla

/**
 * Created by crgodsey on 6/2/15.
 */
final class DendriteSegment(var synapses: Map[NeuralNode, Double] = Map.empty)(
    implicit val config: CLA.Config) extends NeuralNode {
  import config._

  var active = false
  var activation = 0
  var receptive = 0
  //var sequenceSegment = false

  def threshold = segmentThreshold

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
      case (node, p) if p < minDistalPermanence =>
        pruned += 1
        false
      case a => true
    }

    pruned
  }

  def update(): Unit = {
    activation = synapses.count {
      case (node, p) =>
        p > connectionThreshold && node.active
    }

    receptive = synapses.count {
      case (node, p) => p > connectionThreshold
    }

    active = activation > threshold
  }
}
