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

  def reinforceAndPrune(): Int = {
    //segments.foreach(_.reinforce())
    val max = mostActive

    /*if(max.activation > 4/*minActivation*/) */max.map { segment =>
      segment.reinforce()
      segment.pruneSynapses()
    } getOrElse 0
  }

  /*def maybeAddSynapse(): Unit = {
    segments.foreach { segment =>
      if(segments)
    }
  }*/

  //def pruneSynapses(): Int = segments.iterator.map(_.pruneSynapses()).sum
}
