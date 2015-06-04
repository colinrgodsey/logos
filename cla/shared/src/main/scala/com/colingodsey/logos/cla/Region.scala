package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.ExtraMath.randomNormal
import com.colingodsey.logos.collections.RollingAverage

import scala.collection.immutable.VectorBuilder
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._

trait DutyCycle extends NeuralNode {
  private var _boost = 0.0

  def parent: DutyCycle.Booster

  def boostIncr: Double
  def activationThreshold: Int
  def activation: Int
  def active: Boolean
  def boostPermanence(): Unit
  def dutyCycleUpdateRatio: Double
  val activeDutyCycle: RollingAverage
  val overlapDutyCycle: RollingAverage

  def boost = _boost
  protected def boost_=(x: Double) = _boost = x

  def overlap: Double = {
    val a = activation

    if(a < activationThreshold) 0.0
    else a * (1.0 + boost)
  }

  //TODO: data from parent, or inhibition radius?
  def updateDutyCycle(): Unit = if(math.random < dutyCycleUpdateRatio) {
    //val maxDutyCycle = neighborsIn(region.inhibitionRadius).map(_.activeDutyCycle.toDouble).max

    val maxDutyCycle = parent.maxDutyCycle
    val minDutyCycle = 0.01 * maxDutyCycle

    activeDutyCycle += (if(active) 1 else 0)
    overlapDutyCycle += (if(overlap >= activationThreshold) 1 else 0)

    if(activeDutyCycle.toDouble < minDutyCycle) boost += boostIncr
    else boost = 0

    //enforce all synapses a small amount
    if(overlapDutyCycle.toDouble <= minDutyCycle)
      boostPermanence()
  }

}

object DutyCycle {
  trait Booster {
    def maxDutyCycle: Double
  }

  trait Inhibitor {

  }
}

trait SDR extends DutyCycle {
  def connectionThreshold: Double
  def permanenceInc: Double
  def permanenceDec: Double
  def minDistalPermanence: Double

  protected var connections: Array[(NeuralNode, Double)]

  def numConnections = connections.length

  def boostPermanence(): Unit = {
    for(i <- 0 until connections.length) {
      val (node, p) = connections(i)

      connections(i) = node -> (p + connectionThreshold * 0.1)
    }
  }

  //TODO: minActivation?
  def reinforce(): Unit = /*if(activation > minActivation)*/ {
    for(i <- 0 until connections.length) {
      val (node, p) = connections(i)

      val newP =
        if (node.active) math.min(1.0, p + permanenceInc)
        else math.max(0.0, p - permanenceDec)

      if(newP != p)
        connections(i) = node -> newP
    }
  }

  //TODO: merge into reinforce?
  def needsPruning = connections.exists {
    case (_, p) if p < minDistalPermanence => true
    case _ => false
  }

  def addConnection(node: NeuralNode, p: Double) =
    connections :+= node -> p

  def pruneSynapses(): Int = if(needsPruning) {
    var pruned = 0

    connections = connections filter {
      case (_, p) if p < minDistalPermanence =>
        pruned += 1
        false
      case _ => true
    }

    pruned
  } else 0
}

class Region(implicit val config: CLA.Config,
    val ec: ExecutionContext = CLA.newDefaultExecutionContext) extends DutyCycle.Booster { region =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  val columns = (0 until regionWidth).map(new Column(region, _)).toVector
  val input = Array.fill(inputWidth)(false)

  var inhibitionRadiusAverage = RollingAverage(dutyAverageFrames)
  var maxDutyCycle = 1.0

  inhibitionRadiusAverage += regionWidth / 2.0

  def inhibitionRadius: Radius = math.max(inhibitionRadiusAverage.toDouble, 3)

  def update(input: Input): Unit = {
    for(i <- 0 until input.length) this.input(i) = input(i)

    val topActive = spatialPooler()
    //columns.foreach(_.temporalPrePooler())
    //TODO: is this really thread safe? prepooler?
    distributedExec(desiredLocalActivity / 4, topActive)(_.temporalPrePooler())
    topActive.foreach(_.temporalPostPooler())
  }

  def columnsNear(loc: Location, rad: Radius) = {
    val min = math.max(0, loc - rad).toInt
    val max = math.min(regionWidth - 1, loc + rad).toInt

    (min to max).iterator map columns
  }

  def numActiveFromPrediction = columns.count(column => column.active && column.wasPredicted)
  def numActive = columns.count(_.active)

  def anomalyScore = 1.0 - numActiveFromPrediction / numActive.toDouble

  def distributedExec[T](chunkSize: Int, items: IndexedSeq[T])(f: T => Unit): Unit = {
    val vectorBuilder = new VectorBuilder[Future[Unit]]
    var i = 0
    var firstChunk: Iterator[T] = null

    while(i < items.length) {
      val chunk = items.iterator.slice(i, i + chunkSize)

      if(firstChunk == null) firstChunk = chunk
      else vectorBuilder += Future(chunk foreach f)

      i += chunkSize
    }

    val futures = vectorBuilder.result()
    val future = Future.sequence(futures)

    //reuse same thread for first chunk
    if(firstChunk != null) firstChunk foreach f

    Await.result(future, 100.seconds)

    ()
  }

  def spatialPooler(): IndexedSeq[Column] = {
    //clear activation state and update input
    columns.foreach { column =>
      column.active = false
      column.proximalDendrite.update()
    }

    //TODO: real inhibition radius? or no?
    val sorted = columns.sortBy { column =>
      //(column => (-column.overlap, -column.activeFromPrediction)
      val pred = if(column.wasPredicted) 1 else 0
      //val pred = column.predicationAverage.toDouble

      (-column.overlap, -pred, column.ordinal)
    }
    val (topK, tail) = sorted.splitAt(desiredLocalActivity)

    //activated top columns within our inhibition radius
    topK.filter(_.overlap > 0).foreach(_.activate())


    /*
    columns.foreach(_.spatialPooler())
*/

    //update rolling averages
    //columns.foreach(_.updateDutyCycle())
    distributedExec(regionWidth / 4, columns)(_.updateDutyCycle())

    maxDutyCycle = columns.maxBy(_.activeDutyCycle.toDouble).activeDutyCycle.toDouble
    inhibitionRadiusAverage += averageReceptiveFieldRadius / inputWidth * regionWidth

    topK
  }

  def averageReceptiveFieldRadius = {
    //TODO: filter or not?
    val filtered = columns//.filter(_.active)

    val sum = filtered.map(_.receptiveFieldRadius).sum

    sum.toDouble / filtered.length
  }

  def seedDistalSynapses(): Unit = {
    columns.foreach(_.seedDistalSynapses())
  }

  def getRandomCell(refColumn: Column, useLearnCell: Boolean): NeuralNode = {
    //TODO: variance based on inhibitionRadius?
    //val columnSel = refColumn.loc + randomNormal(0.5) * columns.length
    val columnSel = math.random * columns.length
    //TODO: wrap on edges via ring, or cut off via a line?

    //TODO: ignore same column... or no?
    if(columnSel < 0 || columnSel >= columns.length) getRandomCell(refColumn, useLearnCell)
    else {
      val column = columns(columnSel.toInt)

      if (column == refColumn) getRandomCell(refColumn, useLearnCell)
      else if(column.wasPredicted && useLearnCell) column.learningCell
      else column.randomCell
    }
  }

  def getRandomProximalPermanence = {
    val s = connectionThreshold * 0.2 //10% variance
    val n = (s * 2 * math.random) - s

    connectionThreshold / 2.0
    //connectionThreshold + n
  }

  def getRandomDistalPermanence = {
    val s = connectionThreshold * 0.1 //10% variance
    val n = (s * 2 * math.random) - s

    connectionThreshold / 2.0
    //connectionThreshold + n
  }
}
