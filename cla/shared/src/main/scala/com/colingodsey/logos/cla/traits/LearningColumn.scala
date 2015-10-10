package com.colingodsey.logos.cla.traits

import com.colingodsey.logos.cla.{Layer, NeuralNode, Addressable, Cell}
import com.colingodsey.logos.collections.RollingAverage

trait MiniColumn extends NeuralNode with Addressable { column =>
  var wasPredicted: Boolean
  var wasActive: Boolean
  def active: Boolean

  //def input: NeuralNode
  val layer: Layer

  def ordinal: Double

  def cells: Seq[NeuralNode]
}

trait LearningColumn extends MiniColumn { column =>
  val layer: SequenceLayer

  import layer.config
  import config._

  def loc: layer.Location

  val cells: IndexedSeq[Cell] = (0 until columnHeight).map(
    idx => new Cell(column, index = idx)).toArray[Cell]

  val uuid = math.random

  var active = false
  @volatile private var selectedLearningCell: Option[Cell] = None
  @volatile private var nextLearningCell: Option[Cell] = None
  var wasPredicted = false
  var wasActive = false

  def randomCell = cells((cells.length * math.random).toInt)
  def learningCell = selectedLearningCell.getOrElse(randomCell)
  def activeCells = cells.filter(_.active)

  def cellIndexes = 0 until columnHeight

  def ordinal = uuid

  override def toString = {
    val active = cells.map {
      case cell if cell.active && wasPredicted => "P"
      case cell if cell.active => "A"
      case _ => "O"
    }.mkString

    s"$active"
  }

  //def receptiveFieldSize = proximalDendrite.receptive
  //def receptiveFieldRadius = inputSegment.receptiveRadius(loc)

  def leastPredictiveDutyCell =
    cells.minBy(x => (x.leastPredictiveDutyCycle, x.activeOrdinal))
  def mostPredictiveDutyCell =
    cells.maxBy(x => (x.mostPredictiveDutyCycle, x.activeOrdinal))

  /**
   * Calculate predictive state here.
   * No modification should be made to any cells or columns here!
   * We should just predict what happens in the post pooler.
   */
  def temporalPrePooler(): Unit = if(active) {
    cells.foreach(_.computePredictive())

    val hasPredictive = cells.exists(_._predictive)

    /*
    STICKY CELL HYPOTHESIS
    is it best to stick with one learning cell per column until
    it becomes relevent. This is because the learning cell is selected
    to learn prediction patterns as well as selected for predicting
    the next pattern. Therefor it is important this cell becomes
    relevent at some point.
     */

    //only select new learning cell once old one is predicted or new one predicted
    nextLearningCell = if(learningCell._predictive || hasPredictive) {
      val max = cells.maxBy(_.activationOrdinal)

      Some(max)
    } else Some(learningCell)

    /*
    Calculate any distal mutations here. These calculations
    should NOT affect any of the data used by other columns in
    the pre pooler.
     */
    if(hasPredictive) {
      //cells.filter(_.predictive).foreach(_.reinforceDistal())
      nextLearningCell.getOrElse(randomCell).reinforceDistal()
    } else {
      //only reinforce the 'learning cell' here (max predication)
      nextLearningCell.getOrElse(randomCell).reinforceDistal()
    }
  }

  /**
   * This handles actually mutating cell/column state
   * based on what was predicted in the pre pooler.
   */
  def temporalPostPooler(): Unit = {
    cells.foreach(_.tickDown())

    //TODO: only 1 cell? or many?
    wasPredicted = if(active) {
      //val nPredictive = cells.count(_.activateIfPredicted(burstCellDuration))

      //TODO: wasPredicted only if active this round?
      cells.exists(_.predictive)
    } else false

    //burst cells if not predicted
    if (!wasPredicted && active)
      cells.foreach(_.activate(burstCellDuration))

    if(active) {
      selectedLearningCell = nextLearningCell
      //selectedLearningCell.foreach(_.activateIfPredicted())
      selectedLearningCell.foreach(_.activate(learningCellDuration))
    }
  }

  protected def getItem(item: String): Addressable = {
    val idx = item.toInt

    if(idx >= cells.length) sys.error("index to large for cells")

    cells(idx)
  }
}
