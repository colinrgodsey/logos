package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.LearningColumn
import com.colingodsey.logos.collections.RollingAverage

//TODO: when should we clear 'predictive' ?
//TODO: actual distal segments, not just the 1 fixed one
//TODO: this cell shares the same dendrite structure and weighting as another cell
final class Cell(val column: LearningColumn, index: Int) extends NeuralNode with Addressable { cell =>
  import column.layer
  import layer.config
  import config._
  import layer.Location

  val distalDendrite = /*if(isShadowing) newShadowDendrite else */new DistalDendrite[Location](column.loc)

  lazy val id = index.toString

  var _predictive = false
  private var _active = false
  var activeForTicks = 0

  var activeOrdinal = math.random

  def predictive = _predictive
  def active = _active

  //TODO: verticle offsets?
  def loc = column.loc

  def computePredictive(): Unit = {
    distalDendrite.update()

    _predictive = distalDendrite.active
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
    activeOrdinal = math.random

    _active = true
    //predictive = false
    //TODO: reset, or min?
    activeForTicks = math.max(forTicks, activeForTicks)
  }

  //TODO: count receptive, or no?
  //def predication = distalDendrite.mostActive.map(s => s.activation) getOrElse 0
  def activationOrdinal: (Double, Double, Double) =
    distalDendrite.mostActive.map(_.activationOrdinal) getOrElse (0.0, 0.0, activeOrdinal)

  //def randomSegment = distalDendrite.segments((math.random * distalDendrite.segments.length).toInt)



  def addNewSegment(): Unit = {
    val learningCells = column.layer.getLearningNodes(cell.column)

    distalDendrite.createNewSegment(learningCells)
  }

  //TODO: after adding cell, add a 'cooldown' period
  def addLearningCellToSegment(segment: DendriteSegment): Unit = if(!segment.isFull) {
    val learningCells = column.layer.getLearningNodes(cell.column).filter(!segment.connectedTo(_))

    distalDendrite.fillSegment(segment, learningCells take 1)
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

        //TODO: we really need better logic on how to remove things =\
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
