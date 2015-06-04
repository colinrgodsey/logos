package com.colingodsey.logos.cla

//cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
final class DistalDendrite(val loc: CLA.Location)(implicit val config: CLA.Config) extends NeuralNode with DutyCycle.Booster {
  import config._
  
  var active = false
  var segments = IndexedSeq[DendriteSegment]()
  var synapseFillPercent = 100000
  var mostActive: Option[DendriteSegment] = None
  var maxDutyCycle = 1.0

  //def averageActiveDuty = segments.iterator.map(_.activeDutyCycle.toDouble).sum / segments.length
  def leastActiveDuty = segments.minBy(x => (x.activeDutyCycle.toDouble, math.random))
  def mostActiveDuty = segments.maxBy(x => (x.activeDutyCycle.toDouble, math.random))

  def update(): Unit = {
    segments.foreach(_.update())
    synapseFillPercent = segments.iterator.map(_.numConnections).sum / (segments.length * seededDistalConnections)

    //TODO: min threshold?
    mostActive = //segments.toStream.sortBy(-_.activation).headOption
      if(segments.isEmpty) None
      else Some(segments.maxBy(_.activationOrdinal))

    active = segments.exists(_.active)

    maxDutyCycle = mostActiveDuty.activeDutyCycle.toDouble
  }


  //TODO: min activation?
  def reinforceAndPrune(): Int = {
    //segments.foreach(_.reinforce())
    val max = mostActive

    /*leastActiveDuty.reinforce()
    val leastPruned = leastActiveDuty.pruneSynapses()*/
    val leastPruned = 0

    val mostPruned = /*if(max.activation > 4/*minActivation*/) */max.map { segment =>
      segment.reinforce()
      segment.pruneSynapses()
    } getOrElse 0

    leastPruned + mostPruned
  }
}
