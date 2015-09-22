package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.MiniColumn

import scala.collection.mutable
import scala.concurrent.ExecutionContext

import DefaultShadow._

import scala.reflect.ClassTag
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
      desiredActivityPercent: Double = 0.06,
      columnHeight: Int = 32,
      columnDutyCycleRatio: Double = 0.5,

      inputWidth: Int = 256,
      inputConnectedPercent: Double = 0.10,
      inputRangeSpreadPercent: Double = 0.15,//0.25,//0.15,
      overlapPercent: Double = 0.10, //percent of input connections per column

      segmentThresholdPercent: Double = 0.70, //percent of seededDistalPercent
      seededDistalPercent: Double = 0.10, //percent of columns over desiredLocalActivity
      maxDistalDendrites: Int = 64,
      minDistalPermanence: Double = 0.01,
      segmentDutyCycleRatio: Double = 0.2,
      appendDistalFrequency: Double = 0.08, //chance that an active segment will make a new connection

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
      dynamicInhibitionRadiusScale: Double = 4.0,

      specificNumWorkers: Option[Int] = None
  ) {
    val inputConnectionsPerColumn = (inputWidth * inputConnectedPercent).toInt
    val minOverlap = math.ceil(inputConnectionsPerColumn * overlapPercent).toInt
    val numWorkers = specificNumWorkers getOrElse sys.runtime.availableProcessors()
    val inputRangeRadius = inputRangeSpreadPercent * inputWidth / 2.0
    val nonLocalizedInput = inputRangeSpreadPercent >= 1.0
    val desiredLocalActivity = {
      val x = math.ceil(regionWidth * desiredActivityPercent).toInt
      math.max(x, 4)
    }
    val seededDistalConnections = math.ceil((1 + seededDistalPercent) * desiredLocalActivity + 1).toInt
    val segmentThreshold = math.ceil(segmentThresholdPercent * seededDistalConnections).toInt

    /*require(segmentThreshold >= desiredLocalActivity,
      s"segmentThreshold($segmentThreshold) < desiredLocalActivity ($desiredLocalActivity). " +
          "This may cause self-predicting loops in an area.")*/

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
    
    val numColumns = topology.numPoints(regionWidth)
    val numCells = numColumns * columnHeight
  }

  val DefaultConfig = Config()
  val ReducedConfig = DefaultConfig.copy(
    //columnHeight = 16,
    regionWidth = 128
  )

  type Input = IndexedSeq[Boolean]

  trait InputSource {
    def width: Int
    def iterator: Iterator[Boolean]

    def produce: IndexedSeq[Boolean] = {
      val builder = mutable.ArrayBuilder.make[Boolean]
      builder.sizeHint(width)

      builder ++= iterator

      val res = builder.result()

      require(res.length == width)

      res
    }
  }
  
  object Topology {
    def fixDistanceForWrapping(dist: Double, width: Int): Double = {
      var d = dist

      //clamp to [0, width) first
      while(d > width) d -= width

      //furthest distance any 2 points can be from each other is 1/2 width
      if(d > width / 2) d = width - d

      d
    }
  }

  trait Topology[L] {
    type Location = L

    def numPoints(width: Int): Int
    
    def locationsNear(loc: Location, rad: Double, width: Int): Iterator[Location]
    def distance(a: Location, b: Location, width: Int): Double
    def dims: Int
    def columnIndexFor(loc: Location, width: Int): Int
    def columnLocationFor(idx: Int, width: Int): Location
    def indexFor(loc: Location, width: Int): Int
    def scale(loc: Location, s: Double): Location
    def normalizedRandomLocations(loc: Location, rad: Double, width: Int,
        r: Random = Random): Stream[Location]
    def uniqueNormalizedLocations(loc: Location, rad: Double, width: Int): Stream[Location]
    def radiusOfLocations(locations: TraversableOnce[L], width: Int): Double

    def columnIndexesNear(loc: Location, rad: Double, width: Int): Iterator[Int] =
      locationsNear(loc, rad, width).map(columnIndexFor(_, width))

    def randomLocationsNear(loc: Location, rad: Double, width: Int): Stream[Location] = {
      val seq = locationsNear(loc, rad, width).toStream

      seq.sortBy(_ => math.random).toStream
    }

    def randomLocationNear(loc: Location, rad: Double, width: Int): Location =
      randomLocationsNear(loc, rad, width).head

    def columnIndexFor(loc: Location)(implicit cfg: Config[Location]): Int =
      columnIndexFor(loc, cfg.regionWidth)

    def columnLocationFor(idx: Int)(implicit cfg: Config[Location]): Location =
      columnLocationFor(idx, cfg.regionWidth)

    trait LocalNeuralNode extends NeuralNode with NeuralNode.Local[Location]
  }

  trait OneDTopology extends Topology[Int] {
    def isLocationValid(loc: Location, width: Int): Boolean

    def columnIndexFor(loc: Location, width: Int): Int =
      indexFor(loc, width)

    def dims = 1

    def numPoints(width: Int): Int = width

    def columnLocationFor(idx: Int, width: Int): Location = idx

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

      inner.filter(isLocationValid(_, width)).distinct
    }

    def radiusOfLocations(locations: TraversableOnce[Location], width: Int): Double = {
      var min = Double.MaxValue
      var max = 0

      locations.foreach { location =>
        if(location < min) min = location
        if(location > max) max = location
      }

      (max - min) / 2.0
    }

    //1st standard deviation. 70% within rad
    def normalizedRandomLocations(loc: Location, rad: Double, width: Int, r: Random): Stream[Location] = {
      val x: Location = (r.randomNormal * rad + loc).toInt

      def next = normalizedRandomLocations(loc, rad, width)

      if(isLocationValid(x, width)) x #:: next
      else next
    }
  }

  case class CylinderTopology(height: Int) extends Topology[(Int, Int)] {
    private implicit def t2ToV2(tup2: Location) = Vec2(tup2._1, tup2._2)
    
    private implicit def V2Tot2(pos: Vec2): Location = (pos.x.toInt, pos.y.toInt)

    val maxY = height / 2
    val minY = -maxY

    def numPoints(width: Int): Int = width * height

    def isValidLocation(loc: Location): Boolean = {
      val y = loc._2

      y >= minY && y < maxY
    }

    def locationsNear(loc: Location, rad: Double, width: Int): Iterator[Location] = {
      val (x, y) = loc
      
      val radInt = math.ceil(rad).toInt

      val minX = x - radInt
      val maxX = x + radInt
      val minY = y - radInt
      val maxY = y + radInt
      
      for {
        x <- (minX to maxX).iterator
        y <- minY to maxY
        pos = (x, y)
        if isValidLocation(pos)
        d = (pos - loc).length
        if d <= rad
      } yield pos
    }

    def radiusOfLocations(locations: TraversableOnce[Location], width: Int): Double = {
      val locStream = locations.toStream.map(l => l: Vec2)

      val sum = locStream.sum
      val center = sum / locStream.length

      locStream.iterator.map(p => (p - center).length).max
    }

    def distance(a: Location, b: Location, width: Int): Double = {
      val d = (a - b).length

      Topology.fixDistanceForWrapping(d, width)
    }

    def dims: Int = 2

    def columnLocationFor(idx: Int, width: Int): Location = {
      val y = idx / width
      val x = (idx - y * width)

      (x, y)
    }

    def columnIndexFor(loc: Location, width: Int): Int =
      loc._2 * width + loc._1

    def uniqueNormalizedLocations(loc: Location, rad: Double, width: Int): Stream[Location] = ???

    def normalizedRandomLocations(loc: Location, rad: Double, width: Int, r: Random): Stream[Location] = ???

    def indexFor(loc: Location, width: Int): Int = ???

    def scale(loc: Location, s: Double): Location = ???
  }

  case object LineTopology extends OneDTopology {
    def locationsNear(loc: Location, rad: Double, width: Int): Iterator[Location] = {
      val radInt = math.ceil(rad).toInt
      
      val min = loc - radInt
      val max = loc + radInt

      (min to max).iterator.filter(isLocationValid(_, width))
    }

    def distance(a: Int, b: Int, width: Int): Double =
      math.abs(a - b).toDouble

    def indexFor(x: Location, width: Int): Int = {
      require(isLocationValid(x, width), s"invalid line location $x of $width!")

      x
    }

    def isLocationValid(loc: Location, width: Int): Boolean =
      loc >= 0 && loc < width
  }

  case object RingTopology extends OneDTopology {
    def locationsNear(loc: Location, rad: Double, width: Int): Iterator[Location] = {
      val radInt = math.ceil(rad).toInt

      val min = loc - radInt
      val max = loc + radInt

      (min to max).iterator
    }

    def isLocationValid(loc: Location, width: Int): Boolean = true
    
    def distance(a: Int, b: Int, width: Int): Double = {
      val d = math.abs(a - b).toDouble

      Topology.fixDistanceForWrapping(d, width)
    }

    override def radiusOfLocations(locations: TraversableOnce[Location], width: Int): Double = {
      val d = super.radiusOfLocations(locations, width)

      Topology.fixDistanceForWrapping(d, width)
    }

    def indexFor(loc: Location, width: Int): Int = {
      var x = loc

      //yeah, im a bad ass. I know it.
      while(x < 0) x += width
      while(x >= width) x -= width

      x
    }
  }

  private object _VM {
    //shadow from on high
    import shadow._

    val VM = VMImpl
  }

  val VM = _VM.VM
}

trait Addressable {
  def id: String

  protected def getItem(item: String): Addressable

  def get(path: String): Addressable = get(path.split(".").toList)

  def get(path: Seq[String]): Addressable = path match {
    case Nil => this
    case head :: tail => getItem(head).get(tail)
  }
}

trait Layer extends Addressable with CLA.InputSource {
  implicit val config: CLA.Config[_]

  type ColumnType <: MiniColumn

  def columns: IndexedSeq[ColumnType]

  protected def getItem(item: String): Addressable = columns(item.toInt)

  def width = config.numCells

  def iterator = for {
    column <- columns.iterator
    cell <- column.cells
  } yield cell.active
}


object DefaultShadow {

  //to be replaced by shadow context
  object VMImpl {
    def newDefaultExecutionContext: ExecutionContext = ???

    def distributedExec[T](chunkSize: Int, items: Iterable[T])(f: T => Unit): Unit = ???

    def newNativeArray[T: ClassTag](length: Int): mutable.IndexedSeq[T] = ???
  }

}

object NeuralNode {
  trait Local[L] {
    def loc: L
  }

  trait Mutable {
    def activate(): Unit
    def deactivate(): Unit
  }

  trait ActivationOrdinal {
    def activationOrdinal: (Double, Double, Double)
  }

  trait Ordinal {
    def ordinal: Double
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