package com.colingodsey.logos.cla

trait Layer extends DutyCycle.Booster {
  implicit val config: CLA.Config[_]

  type ColumnType <: MiniColumn

  def columns: IndexedSeq[ColumnType]
}

trait L4Layer[L] extends SequenceLayer {
  type Location = L

  implicit val config: CLA.Config[L]

  def getLearningMotorNodes: Stream[Cell] = ???
  def getLearningSensorNodes: Stream[Cell] = ???
}

trait L3Layer[L] extends SequenceLayer {
  override type ColumnType = L3Column[L]
  type Location = L

  implicit val config: CLA.Config[L]

  def inhibitionRadius: Double

  def getLearningCells: Stream[Cell] = {
    columns.toStream.sortBy { c =>
      (!c.wasPredicted, !c.wasActive, c.ordinal)
    }.map(_.learningCell)
  }
}

