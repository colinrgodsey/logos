package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.ExtraMath.randomNormal
import com.colingodsey.logos.collections.RollingAverage

class Region(implicit val config: CLA.Config) { region =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  val columns = (0 until regionWidth).map(new Column(region, _)).toVector
  val input = Array.fill(inputWidth)(false)

  var inhibitionRadiusAverage = RollingAverage(dutyAverageFrames)

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

  def numActiveFromPrediction = columns.count(column => column.active && column.activeFromPrediction)
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
      val pred = if(column.activeFromPrediction) 1 else 0

      (-column.overlap, -pred)
    }
    val (topK, tail) = sorted.splitAt(desiredLocalActivity)

    //activated top columns within our inhibition radius
    topK.filter(_.overlap > 0).foreach(_.activate())


    /*
    columns.foreach(_.spatialPooler())

*/

    //update rolling averages
    columns.foreach(_.updateDutyCycle())

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
      else if(column.activeFromPrediction && useLearnCell) column.learningCell
      else column.cells((column.cells.length * math.random).toInt)
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
