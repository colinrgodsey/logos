package com.colingodsey.logos.cla

//cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
final class DistalDendrite(val loc: CLA.Location)(implicit val config: CLA.Config) extends NeuralNode {
  var active = false
  var segments = IndexedSeq[DendriteSegment]()

  //TODO: minThreshold?
  def mostActive = //segments.toStream.sortBy(-_.activation).headOption
    if(segments.isEmpty) None
    else Some(segments.maxBy(s => (s.activation, s.potentialActivation)))

  def update(): Unit = {
    segments.foreach(_.update())

    active = segments.exists(_.active)
  }

  def reinforce(): Unit = {
    //segments.foreach(_.reinforce())
    val max = mostActive

    /*if(max.activation > 4/*minActivation*/) */max.foreach(_.reinforce())
  }

  /*def maybeAddSynapse(): Unit = {
    segments.foreach { segment =>
      if(segments)
    }
  }*/

  def pruneSynapses(): Int = segments.iterator.map(_.pruneSynapses()).sum
}
