package com.colingodsey.logos.cla

trait SequenceLayer extends Layer {
  import com.colingodsey.logos.cla.CLA._

  type Location

  implicit val config: Config[Location]

  def update(): Unit
  def getLearningCells: Stream[Cell]
  //def input(idx: Int): Boolean
  //def inhibitionRadius: Double

  def numActiveFromPrediction = columns.count(column => column.active && column.wasPredicted)
  def numActive = columns.count(_.active)

  def anomalyScore = {
    val na = numActive.toDouble

    if(na == 0) 0.0
    else 1.0 - numActiveFromPrediction / na
  }

  def getLearningCells(c: LearningColumn): Stream[Cell]  =
    getLearningCells.filter(_.column != c)

  def activeColumns = columns.iterator.filter(_.active)
}
