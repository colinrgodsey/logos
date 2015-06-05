package com.colingodsey.logos.cla

import scala.concurrent.ExecutionContext

import DefaultShadow._

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
      dutyAverageFrames: Int = 40,

      specificNumWorkers: Option[Int] = None
  ) {
    require(minOverlap < inputConnectionsPerColumn, "overlap must be greater than possible connections")

    val numWorkers = specificNumWorkers getOrElse sys.runtime.availableProcessors()
  }

  val DefaultConfig = Config()

  type Input = {
    def length: Int
    def apply(idx: Int): Boolean
    def toSeq: Seq[Boolean]
  }

  type Location = Int

  type Radius = Double

  private object _VM {
    //shadow from on high
    import shadow._

    val VM = VMImpl
  }

  val VM = _VM.VM
}


object DefaultShadow {

  //to be replaced by shadow context
  object VMImpl {
    def newDefaultExecutionContext: ExecutionContext = ???

    def distributedExec[T](chunkSize: Int, items: IndexedSeq[T])(f: T => Unit): Unit = ???
  }

}

trait NeuralNode {
  private var _numOutputs = 0

  def active: Boolean

  def loc: CLA.Location

  def numOutputs = _numOutputs

  def connectOutput(): Unit = _numOutputs += 1
  def disconnectOutput(): Unit = _numOutputs -= 1
}

final class NodeAndPermanence(var node: NeuralNode, var p: Double)







