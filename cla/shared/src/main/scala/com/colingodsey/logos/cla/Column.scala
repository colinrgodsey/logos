package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.collections.ExtraMath.randomNormal

trait ColumnX {

}

final class Column(val region: Region, val loc: CLA.Location) extends DutyCycle { column =>
  import region.config
  import config._

  val cells = Array.fill(columnHeight)(new Cell(column))
  val predicationAverage = RollingAverage(dutyAverageFrames)
  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random)
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random)

  val uuid = math.random

  var active = false
  var proximalDendrite = createProximalDendrite
  @volatile var selectedLearningCell: Option[Cell] = None
  var wasPredicted = false
  var wasActive = false

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

  def parent = region
  def activationThreshold = minOverlap
  def boostIncr = config.boostIncr
  def activation = proximalDendrite.activation
  def dutyCycleUpdateRatio = config.columnDutyCycleRatio

  def randomCell = cells((cells.length * math.random).toInt)
  def learningCell = selectedLearningCell.getOrElse(randomCell)

  def ordinal = uuid

  def input(idx: Int) = region.input(inputMap(idx))

  override def toString = {
    val active = cells.map {
      case cell if cell.active && wasPredicted => "P"
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

      new NodeAndPermanence(node, getRandomProximalPermanence)
    })

    new DendriteSegment(loc, region, nodes.toArray.toIndexedSeq)
  }

  //def receptiveFieldSize = proximalDendrite.receptive
  def receptiveFieldRadius = proximalDendrite.receptiveRadius

  def neighborsIn(radius: CLA.Radius) = region.columnsNear(loc, radius).filter(_ != column)

  override def updateDutyCycle(force: Boolean): Unit = {
    predicationAverage += predication

    super[DutyCycle].updateDutyCycle(force)
  }

  def boostPermanence(): Unit = proximalDendrite.boostPermanence()

  //only for strict inhibition radius
  def spatialPooler(): Unit = {
    val sorted = neighborsIn(region.inhibitionRadius).toStream.
        map(_.overlap).filter(_ >= minOverlap).sortBy(-_)

    val min = if(sorted.isEmpty) minOverlap
    else sorted.take(desiredLocalActivity).min

    if(overlap >= min) activate()
  }

  def predication = learningCell.activationOrdinal._1

  def updatePermanence(): Unit = {
    proximalDendrite.reinforce()
  }

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

  def activate(): Unit = {
    active = true
    updatePermanence()
  }

}
