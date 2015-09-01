package com.colingodsey.logos.cla

//cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
final class DistalDendrite[L](
    val loc: L)(implicit val config: CLA.Config[L]) extends NeuralNode with DutyCycle.Booster {
  import config._
  
  var active = false
  var segments = IndexedSeq[DendriteSegment]()
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
    segments.foreach { s =>
      s.update()
      s.updateDutyCycle()
    }

    mostActive =
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
  def reinforce(): Unit = {
    mostActive.foreach{
      x =>
        x.reinforce()
        x.updateDutyCycle(force = true)
    }
  }
}
