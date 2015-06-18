package com.colingodsey.logos.cla

import scala.concurrent.ExecutionContext

import DefaultShadow._

import scala.util.Random

import com.colingodsey.logos.collections._

/*
reinforcement learning

each(?) learning cell gets a global reward (-1, 1) at the end of a cycle.
That learning cell also fires a global reward when selected (positive or negative).
Columns are sorted by reward and selected for action
Some columns have no action (critical for idleness)
 */

//TODO: dynamic thresholds?

/*
Future ideas:
  Neurotransmitter-like VecN 'cloud'. Eventual localized distribution of modulator values (VecN?)

  Imagine a cloud of neurotransmitters spreading in a guassian way

 */
object CLA {
  case class Config[L](
      regionWidth: Int = 2048,
      desiredLocalActivity: Int = 40,
      columnHeight: Int = 32,
      columnDutyCycleRatio: Double = 0.5,

      inputWidth: Int = 128,
      //inputConnectionsPerColumn: Int = 20,
      inputRangePercent: Double = 0.1,
      inputRangeSpreadPercent: Double = 0.15,
      minOverlap: Int = 8,

      segmentThreshold: Int = 12,
      seededDistalConnections: Int = 20,
      maxDistalDendrites: Int = 128,
      minDistalPermanence: Double = 0.01,
      segmentDutyCycleRatio: Double = 0.2,

      connectionThreshold: Double = 0.2,
      permanenceInc: Double = 0.1,
      permanenceDec: Double = 0.05,
      learningCellDuration: Int = 1,//4, //in ticks

      boostIncr: Double = 0.05,
      dutyAverageFrames: Int = 20,

      topology: Topology[L] = OneDimensionalTopology,
      dynamicInhibitionRadius: Boolean = false,
      dynamicInhibitionRadiusScale: Double = 0.5,

      specificNumWorkers: Option[Int] = None
  ) {
    val inputConnectionsPerColumn = (inputWidth * inputRangeSpreadPercent).toInt
    require(minOverlap < inputConnectionsPerColumn, "overlap must be greater than possible connections")

    val numWorkers = specificNumWorkers getOrElse sys.runtime.availableProcessors()
    //val inputRange = (inputWidth * inputRangePercent).toInt
    val inputRangeRadius = inputRangeSpreadPercent * inputWidth / 2.0

    implicit val _topology = topology

    def getRandomProximalPermanence = {
      val s = connectionThreshold * 0.2 //10% variance
      val n = (s * 2 * math.random) - s

      //connectionThreshold / 2.0
      connectionThreshold + n
    }

    def getRandomDistalPermanence = {
      val s = connectionThreshold * 0.1 //10% variance
      val n = (s * 2 * math.random) - s

      //connectionThreshold / 2.0
      //connectionThreshold + 0.2
      connectionThreshold + n
    }
    
    val numColumns = math.pow(regionWidth, topology.dims).toInt
  }

  val DefaultConfig = Config()

  type Input = {
    def length: Int
    def apply(idx: Int): Boolean
    def toSeq: Seq[Boolean]
  }

  trait Topology[L] {
    type Location = L

    def locationsNear(loc: Location, rad: Double): Iterator[Location]
    def distance(a: Location, b: Location)(implicit cfg: CLA.Config[L]): Double
    def dims: Int
    def columnIndexFor(loc: Location)(implicit cfg: CLA.Config[L]): Int
    def columnLocationFor(idx: Int): Location
    def indexFor(loc: Location, width: Int): Int
    def scale(loc: Location, s: Double): Location

    def columnIndexesNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[L]): Iterator[Int] =
      locationsNear(loc, rad) map columnIndexFor

    def randomLocationsNear(loc: Location, rad: Double): Stream[Location] = {
      val seq = locationsNear(loc, rad).toIndexedSeq

      seq.sortBy(_ => math.random).toStream
    }

    def uniqueNormalizedLocations(loc: Location, rad: Double): Stream[Location] = {
      def take(stream: Stream[Location], gathered: Set[Location] = Set.empty
            ): Stream[Location] = stream.headOption match {
        case Some(l) if gathered(l) => take(stream.tail, gathered)
        case Some(l) => l #:: take(stream.tail, gathered + l)
        case None => sys.error("no more... somehow ")
      }

      take(normalizedRandomLocations(loc, rad))
    }

    def normalizedRandomLocations(loc: Location, rad: Double,
      r: Random = Random): Stream[Location]

    def randomLocationNear(loc: Location, rad: Double): Location =
      randomLocationsNear(loc, rad).head

    trait LocalNeuralNode extends NeuralNode {
      def loc: Location
    }
  }

  case object OneDimensionalTopology extends Topology[Int] {
    def locationsNear(loc: Location, rad: Double): Iterator[Location] = {
      val min = (loc - rad).toInt
      val max = (loc + rad).toInt

      (min to max).iterator
    }

    def distance(a: Int, b: Int)(implicit cfg: CLA.Config[Int]): Double = {
      var d = math.abs(a - b).toDouble

      while(d > cfg.regionWidth / 2.0) d -= cfg.regionWidth / 2.0

      d
    }

    def dims = 1

    def columnIndexFor(loc: Location)(implicit cfg: CLA.Config[Int]): Int = {
      indexFor(loc, cfg.regionWidth)
    }

    def indexFor(loc: Location, width: Int): Int = {
      var x = loc

      //yeah, im a bad ass. I know it.
      while(x < 0) x += width
      while(x >= width) x -= width

      x
    }

    def columnLocationFor(idx: Int): Location = idx

    def scale(loc: Location, s: Double): Location = (loc * s).toInt

    //1st standard deviation. 70% within rad
    def normalizedRandomLocations(loc: Location, rad: Double, r: Random): Stream[Location] = {
      val x: Location = (r.randomNormal * rad + loc).toInt

      x #:: normalizedRandomLocations(loc, rad)
    }
  }

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

  def numOutputs = _numOutputs

  def connectOutput(): Unit = _numOutputs += 1
  def disconnectOutput(): Unit = _numOutputs -= 1
}

final class NodeAndPermanence(val node: NeuralNode, var p: Double)

object NodeAndPermanence {
  def unapply(x: Any): Option[(NeuralNode, Double)] = x match {
    case x: NodeAndPermanence => Some(x.node, x.p)
    case _ => None
  }
}







