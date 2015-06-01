package com.colingodsey.logos.collections

import scala.collection.mutable

object FinalCLA {
  case class Config(
    segmentThreshold: Int = 13,
    minOverlap: Int = 13,
    seededDistalConnections: Int = 20,
    connectionThreshold: Double = 0.2,
    maxDistalDendrites: Int = 5,
    columnHeight: Int = 32,
    regionWidth: Int = 2048,
    inputWidth: Int = 128,
    inputConnectionsPerColumn: Int = 64,
    dutyAlpha: Double = 0.9,
    boostIncr: Double = 0.05,
    desiredLocalActivity: Int = 40,
    permanenceInc: Double = 0.01,
    permanenceDec: Double = 0.005
  ) {

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

case class ScalarEncoder(val length: Int, size: Int, max: Double = 1.0) {//extends FinalCLA.Input {
  def encode(x: Double): FinalCLA.Input = new AnyRef {
    val value = math.max(0, x) / max
    val areaMax = (length * value + size).toInt
    val areaMin = (length * value - size).toInt

    def length = ScalarEncoder.this.length
    def apply(idx: Int) = idx < areaMax && idx >= areaMin

    def toSeq = for(i <- 0 until length) yield apply(i)
  }
}

trait NeuralNode {
  def active: Boolean
}

case class RollingAverage(
    average: Double = 0, count: Int = 0)(val max: Int) {

  def +(n: Double) = {
    val newCount = math.min(max, count + 1)
    val newAverage = (average * (newCount - 1) + n) / newCount

    copy(average = newAverage, count = newCount)(max)
  }

  def toDouble = average
}

class Region(val config: FinalCLA.Config) { region =>
  import config._
  import FinalCLA._

  val columns = (0 until regionWidth).map(new Column(_)).toVector
  val regionInput = Array.fill(inputWidth)(false)

  var inhibitionRadius: Radius = 1.0

  def update(input: Input): Unit = {
    for(i <- 0 until input.length) regionInput(i) = input(i)

    spatialPooler()
    columns.foreach(_.temporalPrePooler())
    columns.foreach(_.temporalPostPooler())
  }

  def columnsNear(loc: Location, rad: Radius): Stream[Column] = {
    val min = math.max(0, loc - rad).toInt
    val max = math.min(regionWidth - 1, loc + rad).toInt

    (min to max).toStream map columns
  }

  def spatialPooler(): Unit = {
    //clear activation state and update input
    columns.foreach(_.active = false)

    //TODO: real inhibition radius?
    val sorted = columns.sortBy(-_.overlap)
    val (topK, tail) = sorted.splitAt(desiredLocalActivity)

    //activated top columns within our inhibition radius
    topK.filter(_.overlap > 0).foreach(_.activate())

    //update rolling averages
    columns.foreach(_.updateDutyCycle())

    inhibitionRadius = averageReceptiveFieldSize
  }

  def averageReceptiveFieldSize = {
    val sum = columns.map(_.receptiveFieldSize).sum

    sum.toDouble / columns.length
  }

  def seedDistalSynapses(): Unit = {
    columns.foreach(_.seedDistalSynapses())
  }

  def getRandomCell: NeuralNode = {
    val column = columns((columns.length * math.random).toInt)
    column.cells((column.cells.length * math.random).toInt)
  }

  class Column(val loc: Location) { column =>
    val cells = Array.fill(columnHeight)(new Cell)

    var active = false
    var proximalDendrite = createProximalDendrite
    var boost = 0.0
    var activeDutyCycle = RollingAverage()(100)
    var overlapDutyCycle = RollingAverage()(100)

    val cellIndexes = 0 until columnHeight

    val inputMap = {
      var outSet = Set[Int]()
      var out = Seq[Int]()
      var added = 0

      while(added < inputConnectionsPerColumn) {
        val i = (math.random * inputWidth).toInt

        if(!outSet(i)) {
          outSet += i
          out :+= i
          added += 1
        }
      }

      out.toIndexedSeq
    }

    def input(idx: Int) = regionInput(inputMap(idx))

    override def toString = {
      val active = cells.map {
        case cell if cell.active && cell.predictive => "X"
        case cell if cell.predictive => "P"
        case cell if cell.active => "A"
        case _ => "O"
      }.mkString
      active
    }

    def createProximalDendrite = {
      val nodes = (0 until inputConnectionsPerColumn map { idx =>
        val node: NeuralNode = new NeuralNode {
          def active: Boolean = input(idx)
        }

        node -> getRandomPermanence
      }).toMap

      new DendriteSegment(nodes)
    }

    def getRandomPermanence = {
      val s = connectionThreshold * 0.1 //10% variance
      val n = (s * 2 * math.random) - s

      connectionThreshold + n
    }

    def overlap = {
      val activation = proximalDendrite.activation

      (if(activation < minOverlap) 0 else activation) * (1.0 + boost)
    }

    def receptiveFieldSize = proximalDendrite.receptive

    def neighborsIn(radius: Radius): Set[Column] = region.columnsNear(loc, radius).toSet

    def updateDutyCycle(): Unit = {
      val maxDutyCycle = neighborsIn(inhibitionRadius).map(_.activeDutyCycle.toDouble).max
      val minDutyCycle = 0.01 * maxDutyCycle

      activeDutyCycle += (if(active) 1 else 0)
      overlapDutyCycle += overlap

      if(activeDutyCycle.toDouble < minDutyCycle)
        boost += boostIncr
      else boost = 0

      //enforce all synapses a small amount
      if(overlapDutyCycle.toDouble < minDutyCycle) {
        val synapses = proximalDendrite.synapses map {
          case (node, p) => node -> (p + connectionThreshold * 0.1)
        }

        proximalDendrite.synapses = synapses
      }

      inhibitionRadius = averageReceptiveFieldSize / 2.0
    }

    def updatePermanence(): Unit = if(active) {
      proximalDendrite.reinforce()
    }

    //active columns will 'tick' predictive state of cells
    def temporalPrePooler(): Unit = if(active) {
      cells.foreach(_.computePredictive())

      val hasPredictive = cells.exists(_.predictive)

      if(hasPredictive) {
        //TODO: most predictive only
        cells.filter(_.predictive).foreach(_.distalDendrite.reinforce())
      } else {
        cells.foreach(_.distalDendrite.reinforce())
      }
    }

    //TODO: learning cell and sequence segments
    def temporalPostPooler(): Unit = if(active) {
      cells.foreach(_.activateIfPredicted())

      val someActive = cells.exists(_.active)

      /*
      if none active from prediction, activate all
      for our 'context-less' activation.
      Otherwise deactivate all.
      */
      if(!someActive) cells.foreach(_.activate())
    } else {
      //deactivate all
      cells.foreach(_.deactivate())
    }
/*
    def temporalPooler(): Unit = if(active) {
      var predicted = false
      var learningCellChosen = false

      val queue = new mutable.Queue[() => Unit]()

      cells foreach { cell =>
        if(cell.predictive) {
          cell.distalDendrite.mostActive match {
            case Some(x) if x.sequenceSegment =>

              queue += { () =>

              }

            case None =>
          }
        }
      }
    }*/

    def activate(): Unit = {
      active = true
      updatePermanence()
    }

    def seedDistalSynapses(): Unit = for {
      cell <- cells
      nSegments = math.floor(math.random * maxDistalDendrites + 1).toInt
      segments = Seq.fill(nSegments)(new DendriteSegment)
      _ = cell.distalDendrite.segments = segments.toSet
      segment <- segments
      otherCells = Seq.fill(seededDistalConnections)(getRandomCell)
      otherCell <- otherCells
    } segment.synapses += otherCell -> getRandomPermanence

    //TODO: actual distal segments, not just the 1 fixed one
    class Cell extends NeuralNode {
      var predictive = false
      private var _active = false

      val distalDendrite = new DistalDendrite

      def active = _active

      def computePredictive(): Unit = {
        distalDendrite.update()

        predictive = distalDendrite.active
      }

      def deactivate(): Unit = {
        _active = false
        predictive = false
      }

      def activate(): Unit = {
        _active = true
        predictive = false
      }

      def activateIfPredicted(): Unit = {
        if (predictive) activate() else deactivate()
      }
    }
  }

  class DendriteSegment(var synapses: Map[NeuralNode, Double] = Map.empty) extends NeuralNode {
    var active = false
    //var sequenceSegment = false

    def threshold = segmentThreshold

    def receptive = synapses.count {
      case (node, p) => p > connectionThreshold
    }

    def activation = synapses.count {
      case (node, p) =>
        p > connectionThreshold && node.active
    }

    def reinforce(): Unit = {
      synapses = synapses map {
        case (node, p) =>
          val newP =
            if (node.active) math.min(1.0, p + permanenceInc)
            else math.max(0.0, p + permanenceDec)

          node -> newP
      }
    }


    def update(): Unit = {
      active = false

      //we activatd. lets also reinforce
      if(activation > threshold) {
        active = true

        //reinforce()
      }
    }
  }

  //cell via predictive <- OR segments as distal dentrite <- THRESH synapses as segment
  class DistalDendrite extends NeuralNode {
    var active = false
    var segments = Set[DendriteSegment]()

    def mostActive = segments.toStream.sortBy(-_.activation).headOption

    def update(): Unit = {
      segments.foreach(_.update())

      active = segments.exists(_.active)
    }

    def reinforce(): Unit = segments.foreach(_.reinforce())
  }
}