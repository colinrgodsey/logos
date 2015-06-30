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

  def getLearningColumns: Stream[L4Column[L]] =
    columns.toStream.filter(_.feedForwardActive).sortBy(c => (-c.overlap, c.ordinal))

  def getLearningMotorNodes = {
    motorInput.segments.toStream.filter(_.active).sortBy(s => (-s.overlap, math.random))
  }

  //TODO: need to try inhibit columns that dont transition somewhere maybe....
  def inhibitColumns(): Unit = {
    val sorted = columns.sortBy(c => (!c.active, !c.feedForwardActive, -c.activeCount, -c.overlap, c.ordinal))

    sorted.drop(config.desiredLocalActivity).foreach(_.activeCount = 0)
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

