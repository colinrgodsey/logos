package com.colingodsey.logos.cla

import scala.collection.mutable

//cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
final class DistalDendrite[L](
    val loc: L)(implicit val config: CLA.Config[L]) extends NeuralNode {
  import config._
  
  var active = false
  var mostActive: Option[DendriteSegment] = None
  var maxDutyCycle = 1.0

  val segmentBuffer = mutable.Buffer[DendriteSegment]()
  var numSegments = 0

  def noSegments = numSegments == 0

  //def averageActiveDuty = segments.iterator.map(_.activeDutyCycle.toDouble).sum / segments.length
  def leastActiveDuty = if(noSegments) None else Some(segmentsItr.minBy(x => (x.activeDutyCycle.toDouble, x.randomOrdinal)))
  def mostActiveDuty = if(noSegments) None else Some(segmentsItr.maxBy(x => (x.activeDutyCycle.toDouble, x.randomOrdinal)))

  def leastActiveDutyCycle = leastActiveDuty.map(_.activeDutyCycle.toDouble) getOrElse 0.0
  def mostActiveDutyCycle = mostActiveDuty.map(_.activeDutyCycle.toDouble) getOrElse 0.0

  def isFull = numSegments > maxDistalDendrites

  def leastActive = leastActiveDuty

  def segmentsItr = segmentBuffer.iterator
  def segments = segmentsItr.filter(_ != null)

  def createNewSegment(learningCells: Stream[NeuralNode]): Unit = {
    val segment = new DendriteSegment()

    if(learningCells.length < segmentThreshold) return

    fillSegment(segment, learningCells)

    add(segment)
  }

  def fillSegment(segment: DendriteSegment, learningCells: Stream[NeuralNode]): Unit = {
    if(learningCells.nonEmpty && segment.numConnections < config.seededDistalConnections)  {
      val cell = learningCells.head
      segment.addConnection(cell, getRandomDistalPermanence)
      fillSegment(segment, learningCells.tail)
    }
  }

  def update(): Unit = {
    var max = segments.toStream.headOption.getOrElse(null)
    val segmentOrdering = implicitly[Ordering[(Double, Double, Double)]]
    var hasActive = false

    var newMaxDutyCycle = 0.0

    segments.foreach { s =>
      s.update()
      s.updateDutyCycle(maxDutyCycle)

      if(s.active && segmentOrdering.lt(max.activationOrdinal, s.activationOrdinal))
        max = s

      if(s.activeDutyCycle.toDouble > newMaxDutyCycle) newMaxDutyCycle = s.activeDutyCycle.toDouble

      if(s.active) hasActive = true
    }

    maxDutyCycle = newMaxDutyCycle

    mostActive =
      if(segments.isEmpty) None
      else Some(max)

    active = hasActive
  }

  def add(s: DendriteSegment): Unit = {
    var found = false
    var idx = 0

    while(!found && idx < segmentBuffer.length) {
      segmentBuffer(idx) match {
        case null =>
          segmentBuffer(idx) = s
          found = true
        case _ =>
          idx += 1
      }
    }

    if(!found) {
      segmentBuffer append s
    }

    numSegments += 1

    s.update()
    s.updateDutyCycle(maxDutyCycle, force = true)
  }

  def removeSegment(s: DendriteSegment): Unit = {
    val curIdx = segmentBuffer.indexOf(s)

    require(curIdx != -1, "segment not found, cannot remove!")

    segmentBuffer(curIdx) = null

    numSegments -= 1
  }

  //TODO: min activation?
  def reinforce(): Unit = {
    mostActive.foreach {
      x =>
        x.reinforce()
        x.updateDutyCycle(maxDutyCycle, force = true)
    }
  }
}
