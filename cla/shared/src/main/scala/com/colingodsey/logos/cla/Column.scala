package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

final class Column(val region: Region, val loc: CLA.Location) { column =>
  import region.config
  import config._

  val cells = Array.fill(columnHeight)(new Cell(column))

  var active = false
  var proximalDendrite = createProximalDendrite
  var boost = 0.0
  var predicationAverage = RollingAverage(dutyAverageFrames)
  var activeDutyCycle = RollingAverage(dutyAverageFrames)
  var overlapDutyCycle = RollingAverage(dutyAverageFrames)

  var activeFromPrediction = false

  val cellIndexes = 0 until columnHeight

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

  def input(idx: Int) = region.input(inputMap(idx))

  override def toString = {
    val active = cells.map {
      case cell if cell.active && activeFromPrediction => "P"
      case cell if cell.active => "A"
      case _ => "O"
    }.mkString

    s"$active loc: $loc "
  }

  def createProximalDendrite = {
    val nodes = (0 until inputConnectionsPerColumn map { idx =>
      val node: NeuralNode = new NeuralNode {
        def active: Boolean = input(idx)
        def loc: CLA.Location = inputMap(idx)
      }

      (node, region.getRandomProximalPermanence)
    })

    new DendriteSegment(loc, nodes.toArray.toIndexedSeq)
  }

  def overlap = {
    val activation = proximalDendrite.activation + predicationAverage.toDouble / 4.0

    (if(activation < minOverlap) 0 else activation) * (1.0 + boost)
  }

  //def receptiveFieldSize = proximalDendrite.receptive
  def receptiveFieldRadius = proximalDendrite.receptiveRadius

  def neighborsIn(radius: CLA.Radius) = region.columnsNear(loc, radius).filter(_ != column)

  def updateDutyCycle(): Unit = {
    val maxDutyCycle = neighborsIn(region.inhibitionRadius).map(_.activeDutyCycle.toDouble).max
    val minDutyCycle = 0.01 * maxDutyCycle

    predicationAverage += predication
    activeDutyCycle += (if(active) 1 else 0)
    overlapDutyCycle += (if(overlap > minOverlap) 1 else 0)

    if(activeDutyCycle.toDouble < minDutyCycle) boost += boostIncr
    else boost = 0

    //enforce all synapses a small amount
    if(overlapDutyCycle.toDouble <= minDutyCycle) {
      val synapses = proximalDendrite.synapses map {
        case (node, p) => node -> (p + connectionThreshold * 0.1)
      }

      proximalDendrite.synapses = synapses
    }
  }

  //only for strict inhibition radius
  def spatialPooler(): Unit = {
    val sorted = neighborsIn(region.inhibitionRadius).toStream.
        map(_.overlap).filter(_ >= minOverlap).sortBy(-_)

    val min = if(sorted.isEmpty) minOverlap
    else sorted.take(desiredLocalActivity).min

    if(overlap >= min) activate()
  }

  def predication = learningCell.predication._1

  def updatePermanence(): Unit = if(active) {
    proximalDendrite.reinforce()
  }

  //TODO: add random?
  def learningCell = cells.maxBy(_.predication)

  //active columns will 'tick' predictive state of cells
  def temporalPrePooler(): Unit = if(active) {
    cells.foreach(_.computePredictive())

    val hasPredictive = cells.exists(_.predictive)

    //TODO: only learn one segment at a time?
    //TODO: only new synapses to learning cells?
    if(hasPredictive) {
      //TODO: most predictive only, or all predictive?
      cells.filter(_.predictive).foreach(_.reinforceDistal())
      //learningCell.reinforceDistal()
    } else {
      //only reinforce the 'learning cell' here (max predication)
      learningCell.reinforceDistal()
    }
  }

  //TODO: learning cell and sequence segments
  def temporalPostPooler(): Unit = if(active) {
    cells.foreach(_.activateIfPredicted())

    activeFromPrediction = cells.exists(_.active)

    /*
    if none active from prediction, activate all
    for our 'context-less' activation.
    Otherwise deactivate all.
    */
    if(!activeFromPrediction) {
      cells.foreach(_.activate())
    }
  } else {
    //deactivate all
    cells.foreach(_.deactivate())
  }

  def activate(): Unit = {
    active = true
    updatePermanence()
  }

  //TODO: seed favouring locality
  //TODO: multiple connections to same cell?
  def seedDistalSynapses(): Unit = for {
    cell <- cells
    nSegments = math.floor(math.random * maxDistalDendrites + 1).toInt
    segments = Array.fill(nSegments)(new DendriteSegment(column.loc))
    _ = cell.distalDendrite.segments = segments
    segment <- segments
    otherCells = Seq.fill(seededDistalConnections)(region.getRandomCell(column, useLearnCell = false))
    otherCell <- otherCells
  } segment.synapses :+= otherCell -> region.getRandomDistalPermanence


}
