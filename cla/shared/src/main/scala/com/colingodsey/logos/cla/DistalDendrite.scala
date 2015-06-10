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
  def leastActiveDuty = if(segments.isEmpty) None else Some(segments.minBy(x => (x.activeDutyCycle.toDouble, x.ordinal)))
  def mostActiveDuty = if(segments.isEmpty) None else Some(segments.maxBy(x => (x.activeDutyCycle.toDouble, x.ordinal)))

  def leastActiveDutyCycle = leastActiveDuty.map(_.activeDutyCycle.toDouble) getOrElse 0.0
  def mostActiveDutyCycle = mostActiveDuty.map(_.activeDutyCycle.toDouble) getOrElse 0.0

  def isFull = segments.length > maxDistalDendrites

  def leastActive = leastActiveDuty

  def update(): Unit = {
    segments.foreach(_.update())
    //synapseFillPercent = segments.iterator.map(_.numConnections).sum / (segments.length * seededDistalConnections)

    //TODO: min threshold?
    mostActive = //segments.toStream.sortBy(-_.activation).headOption
      if(segments.isEmpty) None
      else Some(segments.maxBy(_.activationOrdinal))

    active = segments.exists(_.active)

    maxDutyCycle = mostActiveDutyCycle
  }

  def removeSegment(s: DendriteSegment): Unit = {
    var found = false

    segments = segments filter {
      case a if a == s =>
        s.removeAllConnections()
        false
      case _ => true
    }
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
      0//segment.pruneSynapses()
    } getOrElse 0

    leastPruned + mostPruned
  }
}
