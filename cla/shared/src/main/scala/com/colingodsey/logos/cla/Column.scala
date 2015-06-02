package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.Math.randomNormal

final class Column(val region: Region, val loc: CLA.Location) { column =>
  import region.config
  import config._

  val cells = Array.fill(columnHeight)(new Cell(column))

  var active = false
  var proximalDendrite = createProximalDendrite
  var boost = 0.0
  var activeDutyCycle = RollingAverage(dutyAverageFrames)
  var overlapDutyCycle = RollingAverage(dutyAverageFrames)

  val cellIndexes = 0 until columnHeight

  val inputMap: IndexedSeq[Int] = {
    var outSet = Set[Int]()
    var out = Seq[Int]()
    var added = 0

    while(added < inputConnectionsPerColumn) {
      //val i = (math.random * inputWidth).toInt
      val i = (randomNormal(0.2) * inputWidth + loc).toInt

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
      case cell if cell.active && cell.predictive => "X"
      case cell if cell.predictive => "P"
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

      (node, region.getRandomPermanence)
    })

    new DendriteSegment(loc, nodes.toArray.toIndexedSeq)
  }

  def overlap = {
    val activation = proximalDendrite.activation

    (if(activation < minOverlap) 0 else activation) * (1.0 + boost)
  }

  //def receptiveFieldSize = proximalDendrite.receptive
  def receptiveFieldSize = proximalDendrite.receptiveRadius

  def neighborsIn(radius: CLA.Radius) = region.columnsNear(loc, radius)

  def updateDutyCycle(): Unit = {
    val maxDutyCycle = neighborsIn(region.inhibitionRadius).map(_.activeDutyCycle.toDouble).max
    val minDutyCycle = 0.01 * maxDutyCycle

    activeDutyCycle += (if(active) 1 else 0)
    overlapDutyCycle += (if(overlap > minOverlap) 1 else 0)

    if(activeDutyCycle.toDouble < minDutyCycle) boost += boostIncr
    else boost = 0

    //enforce all synapses a small amount
    if(overlapDutyCycle.toDouble < minDutyCycle) {
      val synapses = proximalDendrite.synapses map {
        case (node, p) => node -> (p + connectionThreshold * 0.1)
      }

      proximalDendrite.synapses = synapses
    }
  }

  //only for strict inhibition radius
  def spatialPooler(): Unit = {
    val sorted = neighborsIn(region.inhibitionRadius).toStream.sortBy(-_.overlap)

    val min = sorted.take(desiredLocalActivity).map(_.overlap).min

    if(overlap >= min) activate()
  }

  def updatePermanence(): Unit = if(active) {
    proximalDendrite.reinforce()
  }

  def learningCell = cells.maxBy(_.predication)

  //active columns will 'tick' predictive state of cells
  def temporalPrePooler(): Unit = if(active) {
    cells.foreach(_.computePredictive())

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
    otherCells = Seq.fill(seededDistalConnections)(region.getRandomCell(column))
    otherCell <- otherCells
  } segment.synapses :+= otherCell -> region.getRandomPermanence


}
