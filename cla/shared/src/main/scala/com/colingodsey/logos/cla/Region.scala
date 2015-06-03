package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.ExtraMath.randomNormal
import com.colingodsey.logos.collections.RollingAverage

trait DutyCycle extends NeuralNode {
  private var _boost = 0.0

  def parent: DutyCycle.Booster

  def boostIncr: Double
  def activationThreshold: Int
  def activation: Int
  def active: Boolean
  def boostPermanence(): Unit
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
  def updateDutyCycle(): Unit = {
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

  var connections: IndexedSeq[(NeuralNode, Double)]

  def boostPermanence(): Unit = {
    connections = connections map {
      case (node, p) => node -> (p + connectionThreshold * 0.1)
    }
  }

  //TODO: minActivation?
  def reinforce(): Unit = /*if(activation > minActivation)*/ {
    connections = connections map {
      case (node, p) =>
        val newP =
          if (node.active) math.min(1.0, p + permanenceInc)
          else math.max(0.0, p - permanenceDec)

        node -> newP
    }
  }

  def pruneSynapses(): Int = {
    var pruned = 0

    connections = connections filter {
      case (_, p) if p < minDistalPermanence =>
        pruned += 1
        false
      case _ => true
    }

    pruned
  }
}

class Region(implicit val config: CLA.Config) extends DutyCycle.Booster { region =>
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

    spatialPooler()
    columns.foreach(_.temporalPrePooler())
    columns.foreach(_.temporalPostPooler())
  }

  def columnsNear(loc: Location, rad: Radius) = {
    val min = math.max(0, loc - rad).toInt
    val max = math.min(regionWidth - 1, loc + rad).toInt

    (min to max).iterator map columns
  }

  def numActiveFromPrediction = columns.count(column => column.active && column.wasPredicted)
  def numActive = columns.count(_.active)

  def anomalyScore = 1.0 - numActiveFromPrediction / numActive.toDouble

  def spatialPooler(): Unit = {
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
    columns.foreach(_.updateDutyCycle())

    maxDutyCycle = columns.iterator.map(_.activeDutyCycle.toDouble).max
    inhibitionRadiusAverage += averageReceptiveFieldRadius / inputWidth * regionWidth
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
