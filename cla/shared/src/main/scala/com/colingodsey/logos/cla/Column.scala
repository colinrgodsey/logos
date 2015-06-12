package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

import scala.concurrent.ExecutionContext

trait MiniColumn extends NeuralNode { column =>
  var wasPredicted: Boolean
  var wasActive: Boolean
  var active: Boolean

  //def input: NeuralNode
  val layer: SequenceLayer

  def update(): Unit

  def ordinal: Double
}

trait Column extends MiniColumn {
  def receptiveFieldRadius: Double
}

class InputSDR(implicit val config: CLA.Config, ec: ExecutionContext) extends Layer { inputLayer =>
  import CLA._
  import config._

  val segments = (for(i <- 0 until regionWidth) yield createSegment(i)).toArray
  val currentInput = Array.fill(inputWidth)(false)

  var maxDutyCycle = 1.0

  protected def createSegment(loc: Location) = {
    val inputMap: IndexedSeq[Int] = {
      var outSet = Set[Int]()
      var out = Seq[Int]()
      var added = 0

      val inputLoc = loc / regionWidth * inputWidth

      while(added < inputConnectionsPerColumn) {
        val i = (math.random * inputWidth).toInt
        //val i = (randomNormal(0.1) * inputWidth + inputLoc).toInt

        if(i >= 0 && i < inputWidth && !outSet(i)) {
          outSet += i
          out :+= i
          added += 1
        }
      }

      out.toArray
    }

    val nodes = (0 until inputConnectionsPerColumn map { idx =>
      val node: NeuralNode = new NeuralNode {
        def active: Boolean = currentInput(idx)
        def loc: CLA.Location = inputMap(idx)
      }

      new NodeAndPermanence(node, getRandomProximalPermanence)
    }).toArray.toIndexedSeq

    new DendriteSegment(loc, inputLayer, nodes)
  }

  def update(input: Input): Unit = {
    for(i <- 0 until input.length) this.currentInput(i) = input(i)

    spatialPooler()
  }

  protected def spatialPooler(): IndexedSeq[DendriteSegment] = {
    //clear activation state and update input
    segments.foreach(_.update())

    //TODO: real inhibition radius? or no?
    val sorted = segments.sortBy { segment =>
      (-segment.overlap, -segment.activation, segment.ordinal)
    }
    val (topK, tail) = sorted.splitAt(desiredLocalActivity)

    //activated top columns within our inhibition radius
    tail.foreach(_.active = false)
    //topK.foreach(x => x.active = x.overlap > 0) //only active inputs

    /*
    columns.foreach(_.spatialPooler())
*/

    //update rolling averages
    //columns.foreach(_.updateDutyCycle())
    VM.distributedExec(regionWidth / numWorkers, segments)(_.updateDutyCycle())

    maxDutyCycle = segments.maxBy(_.activeDutyCycle.toDouble).activeDutyCycle.toDouble
    //inhibitionRadiusAverage += averageReceptiveFieldRadius / inputWidth * regionWidth

    topK
  }
}

final class L3Column(val layer: SequenceLayer, val loc: CLA.Location,
    val inputSegment: DendriteSegment) extends Column { column =>
  import layer.config
  import config._

  val cells = Array.fill(columnHeight)(new Cell(column))
  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random)
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random)

  val uuid = math.random

  var active = false
  @volatile var selectedLearningCell: Option[Cell] = None
  var wasPredicted = false
  var wasActive = false

  def randomCell = cells((cells.length * math.random).toInt)
  def learningCell = selectedLearningCell.getOrElse(randomCell)

  def cellIndexes = 0 until columnHeight

  def ordinal = uuid

  def boost = inputSegment.boost
  def overlap = inputSegment.overlap

  override def toString = {
    val active = cells.map {
      case cell if cell.active && wasPredicted => "P"
      case cell if cell.active => "A"
      case _ => "O"
    }.mkString

    s"$active loc: $loc "
  }

  //def receptiveFieldSize = proximalDendrite.receptive
  def receptiveFieldRadius = inputSegment.receptiveRadius

  def neighborsIn(radius: CLA.Radius) = ??? //layer.columnsNear(loc, radius).filter(_ != column)

  //only for strict inhibition radius
  /*def spatialPooler(): Unit = {
    val sorted = neighborsIn(layer.inhibitionRadius).toStream.
        map(_.overlap).filter(_ >= minOverlap).sortBy(-_)

    val min = if(sorted.isEmpty) minOverlap
    else sorted.take(desiredLocalActivity).min

    if(overlap >= min) activate()
  }*/

  def predication = learningCell.activationOrdinal._1

  def leastPredictiveDutyCell =
    cells.minBy(x => (x.leastPredictiveDutyCycle, x.ordinal))
  def mostPredictiveDutyCell =
    cells.maxBy(x => (x.mostPredictiveDutyCycle, x.ordinal))

  //active columns will 'tick' predictive state of cells
  //TODO: should this fire if active, or if overlap > minOverlap?
  def temporalPrePooler(): Unit = if(active) {
    cells.foreach(_.computePredictive())

    //TOD: this is breaking consistency. updates happening while reffing other selectedLearningCell
    selectedLearningCell = Some(cells.maxBy(_.activationOrdinal))

    val hasPredictive = cells.exists(_.predictive)

    //TODO: only learn one segment at a time?
    //TODO: only new synapses to learning cells?
    if(hasPredictive) {
      //TODO: most predictive only, or all predictive?
      //cells.filter(_.predictive).foreach(_.reinforceDistal())
      learningCell.reinforceDistal()
    } else {
      //only reinforce the 'learning cell' here (max predication)
      learningCell.reinforceDistal()
    }
  }

  //TODO: learning cell and sequence segments
  def temporalPostPooler(): Unit = if(active) {
    cells.foreach(_.activateIfPredicted())

    wasPredicted = cells.exists(_.active)

    /*
    if none active from prediction, activate all
    for our 'context-less' activation.
    Otherwise deactivate all.
    */
    if(!wasPredicted) {
      cells.foreach(_.activate(1))
    }
  } else {
    //deactivate all
    cells.foreach(_.tickDown())
    wasPredicted = cells.exists(_.active)
  }

  def update(): Unit = {
    wasActive = active
    active = inputSegment.active
  }
}
