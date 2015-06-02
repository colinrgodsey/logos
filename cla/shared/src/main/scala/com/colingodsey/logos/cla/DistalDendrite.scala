package com.colingodsey.logos.cla

//cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
final class DistalDendrite(val loc: CLA.Location) extends NeuralNode {
  var active = false
  var segments = IndexedSeq[DendriteSegment]()

  //TODO: minThreshold?
  def mostActive = segments.toStream.sortBy(-_.activation).headOption

  def update(): Unit = {
    segments.foreach(_.update())

    active = segments.exists(_.active)
  }

  def reinforce(): Unit = {
    //segments.foreach(_.reinforce())
    val max = segments.maxBy(_.activation)

    if(max.activation > 4/*minActivation*/) max.reinforce()
  }

  def pruneSynapses(): Int = segments.iterator.map(_.pruneSynapses()).sum
}
