package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits._
import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext
import scala.util.Try





final class Column[L](val layer: TypedSequenceLayer[L], val loc: L,
    val inputSegment: NeuralNode) extends LearningColumn { column =>

  var oldOverlap = 0.0

  lazy val id = loc.toString

  def boost = inputSegment match {
    case x: DutyCycle => x.boost
    case _ => 0.0
  }
  def overlap = inputSegment match {
    case x: DutyCycle => x.overlap
    case _ => 0.0
  }

  def update(): Unit = {
    wasActive = active
    oldOverlap = overlap
    active = inputSegment.active
  }
}
