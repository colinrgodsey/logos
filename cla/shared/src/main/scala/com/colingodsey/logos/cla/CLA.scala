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
      desiredActivityPercent: Double = 0.04,
      columnHeight: Int = 32,
      columnDutyCycleRatio: Double = 0.5,

      inputWidth: Int = 256,
      inputConnectedPercent: Double = 0.05,
      inputRangeSpreadPercent: Double = 0.25,//0.15,
      overlapPercent: Double = 0.30, //percent of input connections per column

      segmentThresholdPercent: Double = 0.70, //percent of seededDistalPercent
      seededDistalPercent: Double = 0.60, //percent of columns over desiredLocalActivity
      maxDistalDendrites: Int = 64,
      minDistalPermanence: Double = 0.01,
      segmentDutyCycleRatio: Double = 0.2,

      connectionThreshold: Double = 0.8,
      permanenceInc: Double = 0.1,
      permanenceDec: Double = 0.05,
      initialPermanenceVariance: Double = 0.3,
      learningCellDuration: Int = 3, //in ticks
      burstCellDuration: Int = 1,

      boostIncr: Double = 0.05,
      dutyAverageFrames: Int = 70,

      topology: Topology[L] = RingTopology,
      dynamicInhibitionRadius: Boolean = true,
      dynamicInhibitionRadiusScale: Double = 1.0,

      specificNumWorkers: Option[Int] = None
  ) {
    val inputConnectionsPerColumn = (inputWidth * inputConnectedPercent).toInt
    val minOverlap = math.ceil(inputConnectionsPerColumn * overlapPercent).toInt
    val numWorkers = specificNumWorkers getOrElse sys.runtime.availableProcessors()
    val inputRangeRadius = inputRangeSpreadPercent * inputWidth / 2.0
    val nonLocalizedInput = inputRangeSpreadPercent >= 1.0
    val desiredLocalActivity = math.ceil(regionWidth * desiredActivityPercent).toInt
    val seededDistalConnections = ((1 + seededDistalPercent) * desiredLocalActivity).toInt
    val segmentThreshold = (segmentThresholdPercent * seededDistalConnections).toInt

    require(segmentThreshold >= desiredLocalActivity,
      s"segmentThreshold($segmentThreshold) < desiredLocalActivity ($desiredLocalActivity). " +
          "This may cause self-predicting loops in an area.")

    implicit val _topology = topology

    def scaleInputsBy(scale: Double) = copy(
      inputWidth = (inputWidth * scale).toInt
    )

    def getRandomProximalPermanence = {
      val s = connectionThreshold * initialPermanenceVariance
      val n = (s * 2 * math.random) - s

      //connectionThreshold / 2.0
      connectionThreshold + n
    }

    def getRandomDistalPermanence = {
      val s = connectionThreshold * initialPermanenceVariance
      val n = (s * 2 * math.random) - s

      //connectionThreshold / 2.0
      //connectionThreshold + n
      connectionThreshold * 1.05
    }
    
    val numColumns = math.pow(regionWidth, topology.dims).toInt
  }

  val DefaultConfig = Config()
  val ReducedConfig = DefaultConfig.copy(
    //columnHeight = 16,
    regionWidth = 128
  )

  type Input = {
    def length: Int
    def apply(idx: Int): Boolean
    def toSeq: Seq[Boolean]
    def iterator: Iterator[Boolean]
  }

  trait InputBase { self: Input =>

  }

  trait Topology[L] {
    type Location = L

    def locationsNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[L]): Iterator[Location]
    def distance(a: Location, b: Location)(implicit cfg: CLA.Config[L]): Double
    def dims: Int
    def columnIndexFor(loc: Location)(implicit cfg: CLA.Config[L]): Int
    def columnLocationFor(idx: Int): Location
    def indexFor(loc: Location, width: Int): Int
    def scale(loc: Location, s: Double): Location
    def normalizedRandomLocations(loc: Location, rad: Double, width: Int,
        r: Random = Random): Stream[Location]
    def uniqueNormalizedLocations(loc: Location, rad: Double, width: Int): Stream[Location]
    def radiusOfLocations(locations: TraversableOnce[L]): Double

    def columnIndexesNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[L]): Iterator[Int] =
      locationsNear(loc, rad) map columnIndexFor

    def randomLocationsNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[L]): Stream[Location] = {
      val seq = locationsNear(loc, rad).toIndexedSeq

      seq.sortBy(_ => math.random).toStream
    }

    def indexFor(loc: Location)(implicit cfg: CLA.Config[L]): Int =
      indexFor(loc, cfg.regionWidth)

    def randomLocationNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[L]): Location =
      randomLocationsNear(loc, rad).head

    trait LocalNeuralNode extends NeuralNode {
      def loc: Location
    }
  }

  trait TwoDTopology extends Topology[Int] {
    def columnIndexFor(loc: Location)(implicit cfg: CLA.Config[Int]): Int =
      indexFor(loc)

    def dims = 1

    def columnLocationFor(idx: Int): Location = idx

    def scale(loc: Location, s: Double): Location = (loc * s).toInt

    //radius as first std-dev [-1,+1]
    /*def probabilityAt(radius: Double, rad: Double, cachedMax: Option[Double] = None): Double = {

    }*/

    def uniqueNormalizedLocations(loc: Location,
        rad: Double, width: Int): Stream[Location] = {

      def inner: Stream[Location] = {
        val σ = rad
        val iMax = rad//0.80/*1.0*/ / ExtraMath.normalPDF(0, σ = σ)

        val outStream = for {
          i <- (0 to (width / 2)).toStream
          prob = ExtraMath.normalPDF(i, σ = σ) * iMax
          x <- if (i == 0) Seq(0) else Seq(i, -i)
          if math.random < prob
        } yield x + loc

        outStream append inner
      }

      inner.distinct
    }

    def radiusOfLocations(locations: TraversableOnce[Location]): Double = {
      var min = Double.MaxValue
      var max = 0

      locations.foreach { location =>
        if(location < min) min = location
        if(location > max) max = location
      }

      (max - min) / 2.0
    }
  }

  case object LineTopology extends TwoDTopology {
    def locationsNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[Int]): Iterator[Location] = {
      val min = (loc - rad).toInt
      val max = (loc + rad).toInt

      (min to max).iterator.filter(x => x >= 0 && x < cfg.regionWidth)
    }

    def distance(a: Int, b: Int)(implicit cfg: CLA.Config[Int]): Double =
      math.abs(a - b).toDouble

    def indexFor(x: Location, width: Int): Int = {
      require(x >= 0 && x < width, "invalid line location!")

      x
    }

    //1st standard deviation. 70% within rad
    def normalizedRandomLocations(loc: Location, rad: Double, width: Int, r: Random): Stream[Location] = {
      val x: Location = (r.randomNormal * rad + loc).toInt

      if(x >= 0 && x < width)
        x #:: normalizedRandomLocations(loc, rad, width)
      else
        normalizedRandomLocations(loc, rad, width)
    }
  }

  case object RingTopology extends TwoDTopology {
    def locationsNear(loc: Location, rad: Double)(implicit cfg: CLA.Config[Int]): Iterator[Location] = {
      val min = (loc - rad).toInt
      val max = (loc + rad).toInt

      (min to max).iterator
    }

    def distance(a: Int, b: Int)(implicit cfg: CLA.Config[Int]): Double = {
      var d = math.abs(a - b).toDouble

      while(d > cfg.regionWidth / 2.0) d -= cfg.regionWidth / 2.0

      d
    }

    def indexFor(loc: Location, width: Int): Int = {
      var x = loc

      //yeah, im a bad ass. I know it.
      while(x < 0) x += width
      while(x >= width) x -= width

      x
    }

    //1st standard deviation. 70% within rad
    def normalizedRandomLocations(loc: Location, rad: Double, width: Int, r: Random): Stream[Location] = {
      val x: Location = (r.randomNormal * rad + loc).toInt

      x #:: normalizedRandomLocations(loc, rad, width)
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