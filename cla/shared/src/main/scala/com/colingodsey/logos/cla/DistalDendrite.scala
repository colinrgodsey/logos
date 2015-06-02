package com.colingodsey.logos.cla

/**
 * Created by crgodsey on 6/2/15.
 */
//cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
final class DistalDendrite extends NeuralNode {
  var active = false
  var segments = IndexedSeq[DendriteSegment]()

  def mostActive = segments.toStream.sortBy(-_.activation).headOption

  def update(): Unit = {
    segments.foreach(_.update())

    active = segments.exists(_.active)
  }

  def reinforce(): Unit = segments.foreach(_.reinforce())

  def pruneSynapses(): Int = segments.iterator.map(_.pruneSynapses()).sum
}
