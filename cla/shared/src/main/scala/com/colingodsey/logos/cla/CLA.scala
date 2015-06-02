package com.colingodsey.logos.cla

/*
Future ideas:
  Neurotransmitter-like VecN 'cloud'. Eventual localized distribution of modulator values (VecN?)
 */
object CLA {
  case class Config(
    segmentThreshold: Int = 13,
    minOverlap: Int = 13,
    seededDistalConnections: Int = 20,
    connectionThreshold: Double = 0.2,
    maxDistalDendrites: Int = 3,
    columnHeight: Int = 32,
    regionWidth: Int = 2048,
    inputWidth: Int = 128,
    inputConnectionsPerColumn: Int = 64,
    dutyAlpha: Double = 0.9,
    boostIncr: Double = 0.05,
    desiredLocalActivity: Int = 40,
    dutyAverageFrames: Int = 10,
    permanenceInc: Double = 0.01,
    permanenceDec: Double = 0.005,
    minDistalPermanence: Double = 0.005
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
}

trait NeuralNode {
  def active: Boolean
}











