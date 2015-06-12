package com.colingodsey.logos.cla

trait SequenceLayer extends Layer {
  import com.colingodsey.logos.cla.CLA._

  type ColumnType <: Column

  def columns: IndexedSeq[ColumnType]
  def getLearningCells: Stream[Cell]
  //def input(idx: Int): Boolean
  def inhibitionRadius: Double

  def columnsNear(loc: Location, rad: Radius) = {
    val min = math.max(0, loc - rad).toInt
    val max = math.min(config.regionWidth - 1, loc + rad).toInt

    (min to max).iterator map columns
  }

  def numActiveFromPrediction = columns.count(column => column.active && column.wasPredicted)
  def numActive = columns.count(_.active)

  def anomalyScore = {
    val na = numActive.toDouble

    if(na == 0) 0.0
    else 1.0 - numActiveFromPrediction / na
  }

  def averageReceptiveFieldRadius = {
    //TODO: filter or not?
    val filtered = columns//.filter(_.active)

    val sum = filtered.map(_.receptiveFieldRadius).sum

    sum.toDouble / filtered.length
  }

  def getLearningCells(c: Column): Stream[Cell]  =
    getLearningCells.filter(_.column != c)

  def activeColumns = columns.iterator.filter(_.active)
}
