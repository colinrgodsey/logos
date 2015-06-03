package com.colingodsey.logos.cla

/*
Future ideas:
  Neurotransmitter-like VecN 'cloud'. Eventual localized distribution of modulator values (VecN?)
 */
object CLA {
  case class Config(
    segmentThreshold: Int = 12,
    minOverlap: Int = 4,
    seededDistalConnections: Int = 32,
    connectionThreshold: Double = 0.2,
    maxDistalDendrites: Int = 128,
    columnHeight: Int = 32,
    regionWidth: Int = 2048,
    desiredLocalActivity: Int = 40,
    inputWidth: Int = 128,
    inputConnectionsPerColumn: Int = 64,
    boostIncr: Double = 0.05,
    dutyAverageFrames: Int = 5,
    permanenceInc: Double = 0.1,
    permanenceDec: Double = 0.05,
    minDistalPermanence: Double = 0.01
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

  def loc: CLA.Location
}











