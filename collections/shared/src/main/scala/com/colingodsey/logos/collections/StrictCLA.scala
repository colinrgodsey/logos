package com.colingodsey.logos.collections

object StrictCLA {
  val distalSegmentThreshold = 15
  val connectionThreshold = 0.2

  class Cell {
    var proximalPermanence = 0.0
    var active = false
    var distalDendrites = Seq.empty[DistalDendrite]

    def predictive = distalDendrites.exists(_.active)
    def output = active || predictive
  }

  //proximal dendrite is some subset of region input
  class ColumnOld {
    var boost = 0.0 //boost input
    val cells = Set.empty[Cell]

    def boostScale = 1 + boost

    def numOutput = cells.count(_.output)

    def connectedProximalCells = for {
      cell <- cells
      if cell.proximalPermanence >= connectionThreshold
    } yield cell

    def process(input: Boolean): Unit = {
      val connected = connectedProximalCells
      val nonConnected = cells -- connected

      nonConnected.foreach(_.active = false)
      connected.foreach(_.active = input)
    }

    def output = numOutput * boostScale
  }

  class Column {

  }


  class DistalDendrite {
    var synapses = Map.empty[Cell, Double]

    def connectedCells = synapses.filter(_._2 > connectionThreshold).keySet
    def activity = connectedCells.count(_.active)
    def active = activity >= distalSegmentThreshold
  }

  trait Region {

  }
}
class StrictCLA {

}
/*
object NewStrictCLA {
  val distalSegmentThreshold = 15
  val connectionThreshold = 0.2

  val regionSize = 20
  val columnHeight = 8

  val columnsPerRegion = regionSize * regionSize
  val cellsPerRegion = columnsPerRegion * columnHeight

  type Input = Array[Double]

  val columns = for {
    x <- 0 until regionSize
    y <- 0 until regionSize
  } yield (x, y)

  class Region { region =>
    case class Cell(x: Int, y: Int, h: Int) {
      def columnIdx = x * regionSize + y
      def idx = columnIdx * columnHeight + h
      def pos = Vec3(x, y, h)

      def active = region.activations(idx)
      def predictive = region.predictions(idx)
      def proximalPermanence = region.proximalPermanence(idx)
    }

    val activations = new Array[Boolean](cellsPerRegion)
    val predictions = new Array[Boolean](cellsPerRegion)
    val proximalPermanence = new Array[Double](cellsPerRegion)
    val distalConnections = Map.empty[Cell, Map[Cell, Double]]

    def cellsForColumn(x: Int, y: Int): Seq[Cell] =
      for(h <- 0 until columnHeight) yield Cell(x, y, h)

    def updatePredictions()
    def clearActivations()

    def spatialInputForColumn(input: Input, x: Int, y: Int): Boolean

    def processInput(input: Input): Unit = {
      updatePredictions()
      clearActivations()

      //activation
      for {
        (x, y) <- columns
        columnInput = spatialInputForColumn(input, x, y)
        cell <- cellsForColumn(x, y)
        isConnected = cell.proximalPermanence > connectionThreshold
      } activations(cell.idx) = isConnected && columnInput
    }
  }
}
*/
object AnotherCLA {
  val segmentThreshold = 15
  val connectionThreshold = 0.2
  val columnHeight = 8
  val regionWidth = 16
  val dutyAlpha = 0.9
  val boostIncr = 0.05
  val numColumns = regionWidth * regionWidth
  val desiredLocalActivity = 10
  val permanenceInc = 0.01
  val permanenceDec = 0.005

  def minOverlap = segmentThreshold

  trait Node /*extends Equals*/ {
    def active: Boolean

    //read incoming Node activations and set our state
    def evaluate(): Unit
  }

  //a thresholding segment
  class DendriteSegment extends Node {
    var active = false
    var connections: Map[Node, Double] = Map.empty

    def connectedNodes = (for {
      (node, permanence) <- connections
      if permanence >= connectionThreshold
    } yield node).toSet

    def evaluate(): Unit = {
      //threshold'd AND
      active = connectedNodes.count(_.active) > segmentThreshold
    }
  }

  class DistalDendrite extends Node {
    var active = false
    var segments = Set[DendriteSegment]()

    //the single and only state change per node
    def evaluate(): Unit = {
      //trigger these manually. they will not be triggered outside this
      segments.foreach(_.evaluate())

      //OR
      active = segments.count(_.active) > 0
    }
  }

  //one for spatial vs temporal i think
  class SingleInputProximalDendrite extends Node { //proximal dendrite
    var active: Boolean = false //manually set by input to region

    //read incoming Node activations and set our state
    def evaluate(): Unit = {}
  }


  class Region { region =>
    val columns = (0 until regionWidth).toArray map { y =>
      (0 until regionWidth).toArray map { x =>
        new Column(x, y)
      }
    }
    val input = new Array[Boolean](columnHeight * numColumns)

    val iterable: Iterable[Column] = for {
      x <- 0 until regionWidth
      y <- 0 until regionWidth
    } yield columns(x)(y)

    val isLearning = true

    def iterator = iterable.iterator

    def poolSpatially(): Unit = {
      iterator.foreach(_.evaluate())
      if(isLearning) {
        iterator.foreach(_.updatePermanence())
        iterator.foreach(_.calculateBoost())
      }
    }

    def poolTemporally(): Unit = {

    }

    def neighbors(pos: Vec2, radius: Double) = for {
      x <- 0 until regionWidth
      if math.abs(x - pos.x) <= radius
      y <- 0 until regionWidth
      if math.abs(y - pos.y) <= radius
      if (Vec2(x, y) - pos).length <= radius
    } yield columns(x)(y)

    def maxDutyCycle(neighbors: Iterable[Column]): Double =
      neighbors.map(_.activeDutyCycle).max

    //Spatial ->> temporal
    class Column(val x: Int, val y: Int) extends Node {
      val proximalSegments = new Array[ProximalDendriteSegment](columnHeight)
      val inputOffset = idx * columnHeight

      var boost = 0.0
      var activeDutyCycle = 0.0
      var overlapDutyCycle = 0.0
      var inhibitionRadius = 1.0
      var active = false

      def boostScale = 1.0 + boost

      def idx = y * columnHeight + x

      def pos = Vec2(x, y)

      def neighbors(radius: Double) = region.neighbors(pos, radius)

      def overlap = {
        val a = proximalSegments.count(_.active)
        (if(a < minOverlap) 0 else a) * boostScale
      }

      def evaluate(): Unit = {
        proximalSegments.foreach(_.evaluate())
      }

      //TODO: verify this actually does something
      def inhibitActivate(): Unit = {
        val inRad = neighbors(inhibitionRadius)
        val mostActive = inRad.toStream.sortBy(-_.overlap).filter(_.overlap > 0)

        val topK = mostActive.take(desiredLocalActivity).map(_.overlap)

        val min = if(topK.isEmpty) 0 else topK.min

        active = overlap > min
      }

      def updatePermanence(): Unit = if(active) proximalSegments.foreach(_.updatePermanence())

      def calculateBoost(): Unit = {
        val minDutyCycle = 0.01 * maxDutyCycle(neighbors(inhibitionRadius))

        //active after inhibition (active)
        activeDutyCycle = activeDutyCycle * (1.0 - dutyAlpha) +
            (if(active) 1 else 0) * dutyAlpha

        if(activeDutyCycle > minDutyCycle) boost = 0
        else boost += boostIncr

        //active before inhibition
        overlapDutyCycle = overlapDutyCycle * (1.0 - dutyAlpha) +
            (if(overlap > 0) 1 else 0) * dutyAlpha

        if(overlapDutyCycle < minDutyCycle) {
          proximalSegments foreach { segment =>
            segment.permanence += connectionThreshold * 0.1
          }
        }

        //should be done in region
        //inhibitionRadius = averageReceptiveFieldSize()
      }

      class ProximalDendriteSegment(val z: Int) extends Node {
        var permanence = 0.0
        var active = false

        def connected = permanence >= connectionThreshold

        def updatePermanence(): Unit = {
          permanence = if(active) math.min(1.0, permanence + permanenceInc)
          else math.max(0.0, permanence + permanenceDec)
        }

        def evaluate(): Unit = {
          active = input(z + inputOffset) && connected
        }
      }
    }
  }

  class Cell extends Node {
    var proximal: Node = _ //input from column
    var distal: Node = _

    var active = false
    var predictive = false

    def output = active || predictive

    def evaluate(): Unit = {
      active = proximal.active
      predictive = distal.active
    }
  }
/*
  class Column {
    var cells: Set[Cell]
  }*/
}