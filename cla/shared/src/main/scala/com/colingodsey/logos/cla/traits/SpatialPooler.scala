package com.colingodsey.logos.cla.traits

import com.colingodsey.logos.cla.CLA._
import com.colingodsey.logos.cla.NeuralNode

object SpatialPooler {
  type Node = DutyCycle with NeuralNode with NeuralNode.Mutable with NeuralNode.ActivationOrdinal
}

trait SpatialPooler[L] {
  val config: Config[L]
  
  import config._

  def neighborsIn(loc: topology.Location, radius: Double): Iterator[SpatialPooler.Node]
  def inhibitionRadius: Double
  def segments: Seq[SpatialPooler.Node]

  def localSpatialPooler(segment: SpatialPooler.Node, loc: topology.Location): Unit =
    if(segment.overlap > 0) {
      val sorted = neighborsIn(loc, inhibitionRadius).filter(_ != segment).
          map(_.overlap).filter(_ > 0).toStream.sortBy(-_)

      val min = if(sorted.isEmpty) 0
      else sorted.take(desiredLocalActivity).min

      val o = segment.overlap
      if(o > min && o > 0) segment.activate()
      else segment.deactivate()
    } else segment.deactivate()

  protected def spatialPooler(): Unit = {
    if(dynamicInhibitionRadius && inhibitionRadius < (inputWidth / 2)) {
      for(i <- 0 until segments.length) {
        val seg = segments(i)
        val loc = topology.columnLocationFor(i)(config)

        localSpatialPooler(seg, loc)
      }
    } else {
      val sorted = segments.sortBy(_.activationOrdinal).toStream.reverse
      val (topK, tail) = sorted.splitAt(desiredLocalActivity)

      //activated top columns within our inhibition radius
      tail.foreach(_.deactivate())
      topK.foreach {
        case x if x.overlap > 0 => x.activate()
        case x => x.deactivate()
      } //only active inputs
    }

    val maxDutyCycle = segments.maxBy(_.activeDutyCycle.toDouble).activeDutyCycle.toDouble

    //update rolling averages
    segments.foreach(_.updateDutyCycle(maxDutyCycle, force = true))
  }
}
