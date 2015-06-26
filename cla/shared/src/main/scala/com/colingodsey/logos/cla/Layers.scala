package com.colingodsey.logos.cla

trait Layer extends DutyCycle.Booster {
  implicit val config: CLA.Config[_]

  type ColumnType <: MiniColumn

  def columns: IndexedSeq[ColumnType]
}

trait L4Layer[L] extends SequenceLayer {
  type Location = L
  override type ColumnType = L4Column[L]

  implicit val config: CLA.Config[L]

  def motorInput: InputSDR[L]

  //TODO: multiple learning cells when bursting?
  def getLearningNodes: Stream[NeuralNode] = Stream.empty

  def getLearningColumn: L4Column[L] = columns.maxBy(c => (c.overlap, math.random))

  def getLearningMotorNodes = {
    motorInput.segments.toStream.filter(_.active).sortBy(s => (s.overlap, math.random))
  }
}

trait L3Layer[L] extends SequenceLayer {
  override type ColumnType = L3Column[L]
  type Location = L

  implicit val config: CLA.Config[L]

  def inhibitionRadius: Double

  //TODO: multiple learning cells when bursting?
  def getLearningNodes: Stream[NeuralNode] = {
    columns.toStream.sortBy { c =>
      (!c.wasPredicted, !c.wasActive, c.ordinal)
    }.map(_.learningCell)
  }
}

