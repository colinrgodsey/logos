package com.colingodsey.logos.cla

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

/*
Future ideas:
  Neurotransmitter-like VecN 'cloud'. Eventual localized distribution of modulator values (VecN?)

  Imagine a cloud of neurotransmitters spreading in a guassian way
 */
object CLA {
  case class Config(
      regionWidth: Int = 2048,
      desiredLocalActivity: Int = 40,
      columnHeight: Int = 32,
      columnDutyCycleRatio: Double = 0.5,

      inputWidth: Int = 128,
      inputConnectionsPerColumn: Int = 64,
      minOverlap: Int = 4,

      segmentThreshold: Int = 12,
      seededDistalConnections: Int = 32,
      maxDistalDendrites: Int = 128,
      minDistalPermanence: Double = 0.01,
      segmentDutyCycleRatio: Double = 0.2,

      connectionThreshold: Double = 0.2,
      permanenceInc: Double = 0.1,
      permanenceDec: Double = 0.05,

      boostIncr: Double = 0.05,
      dutyAverageFrames: Int = 5
  ) {
    require(minOverlap < inputConnectionsPerColumn, "overlap must be greater than possible connections")
  }

  val DefaultConfig = Config()

  type Input = {
    def length: Int
    def apply(idx: Int): Boolean
    def toSeq: Seq[Boolean]
  }

  type Location = Int

  type Radius = Double

  def newDefaultExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)
}

trait NeuralNode {
  def active: Boolean

  def loc: CLA.Location
}

final class NodeAndPermanence(var node: NeuralNode, var p: Double)








