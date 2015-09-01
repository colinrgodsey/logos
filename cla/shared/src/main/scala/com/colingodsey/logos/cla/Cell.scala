package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
//TODO: this cell shares the same dendrite structure and weighting as another cell
final class Cell(val column: LearningColumn, val shadowCell: Option[Cell] = None, index: Int) extends NeuralNode with Addressable { cell =>
  import column.layer
  import layer.config
  import config._

  type Location = layer.Location

  val distalDendrite = /*if(isShadowing) newShadowDendrite else */new DistalDendrite[Location](column.loc)

  lazy val id = index.toString

  var _predictive = false
  private var _active = false
  var activeForTicks = 0

  var ordinal = math.random

  def predictive = _predictive
  def active = _active
  def isShadowing = shadowCell.isDefined

  //TODO: verticle offsets?
  def loc = column.loc

  def computePredictive(): Unit = {
    distalDendrite.update()

    _predictive = distalDendrite.active
  }

  def newShadowDendrite = {

  }

  def leastPredictiveDutyCycle = distalDendrite.leastActiveDutyCycle
  def mostPredictiveDutyCycle = distalDendrite.mostActiveDutyCycle

  def tickDown(): Unit = {
    activeForTicks -= 1

    if(activeForTicks <= 0) deactivate()
  }

  protected def deactivate(): Unit = {
    _active = false
    //predictive = false
  }

  def activate(forTicks: Int): Unit = {
    ordinal = math.random

    _active = true
    //predictive = false
    //TODO: reset, or min?
    activeForTicks = math.max(forTicks, activeForTicks)
  }

  //TODO: count receptive, or no?
  //def predication = distalDendrite.mostActive.map(s => s.activation) getOrElse 0
  def activationOrdinal =
    distalDendrite.mostActive.map(_.activationOrdinal) getOrElse (0.0, 0.0, 0.0, ordinal)

  def randomSegment = distalDendrite.segments((math.random * distalDendrite.segments.length).toInt)

  def fillSegment(segment: DendriteSegment, learningCells: Stream[NeuralNode]): Unit = {
    if(learningCells.nonEmpty && segment.numConnections < config.seededDistalConnections)  {
      val cell = learningCells.head
      segment.addConnection(cell, getRandomDistalPermanence)
      fillSegment(segment, learningCells.tail)
    }
  }

  def addNewSegment(): Unit = {
    val segment = new DendriteSegment(distalDendrite)

    val learningCells = column.layer.getLearningNodes(cell.column)

    if(learningCells.length < segmentThreshold) return

    distalDendrite.segments :+= segment

    fillSegment(segment, learningCells)

    segment.update()
    segment.updateDutyCycle(force = true)
  }

  def addLearningCellToSegment(segment: DendriteSegment): Unit = if(!segment.isFull) {
    val learningCells = column.layer.getLearningNodes(cell.column).filter(!segment.connectedTo(_))

    fillSegment(segment, learningCells take 1)
  }

  def reinforceDistal(): Unit = {
    distalDendrite.reinforce()

    val full = distalDendrite.isFull

    (distalDendrite.active, distalDendrite.mostActive) match {
      case (false, _) if !full =>
        /*distalDendrite.mostActive match {
          case Some(segment) if !segment.isFull =>
            fillSegment(segment, column.layer.getLearningNodes(column))
          case _ =>
            addNewSegment()
        }*/
        addNewSegment()
      case (true, _) if full =>
        val toRemove = distalDendrite.leastActiveDuty

        toRemove.foreach(distalDendrite.removeSegment(_))
      case (true, Some(mostActive)) if math.random < appendDistalFrequency =>
        addLearningCellToSegment(mostActive)
      case _ =>
    }

  }

  def activateIfPredicted(dur: Int = learningCellDuration): Boolean = {
    if (_predictive) activate(dur)

    _predictive
  }

  protected def getItem(item: String): Addressable = item match {
    //case "distal" => distalDendrite
    case x => sys.error("unknown path item " + x)
  }
}
