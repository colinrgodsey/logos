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
  def getLearningNodes: Stream[NeuralNode] = {
    val motorCellsStream = getLearningMotorNodes

    val itr = new Iterator[NeuralNode] {
      val columnCells = getLearningCells.iterator
      val motorCells = motorCellsStream.iterator

      var isOdd = false

      def hasNext: Boolean = columnCells.hasNext || motorCells.hasNext

      def next(): NeuralNode = {
        isOdd = !isOdd

        (columnCells.hasNext, motorCells.hasNext) match {
          case (true, true) if isOdd => columnCells.next()
          case (true, true) => motorCells.next()
          case (true, false) => columnCells.next()
          case (false, true) => motorCells.next()
          case (false, false) => sys.error("No more elements!")
        }
      }
    }

    if(motorCellsStream.length < (config.segmentThreshold / 2)) Stream.empty
    else itr.toStream
  }

  /*def getLearningColumns: Stream[L4Column[L]] =
    columns.toStream.filter(c => c.feedForwardActive || c.active)
        .sortBy(c => (!c.feedForwardActive, -c.overlap, c.ordinal))*/

  //TODO: learning cell, or all active cells?
  def getLearningCells: Stream[Cell] = {
    columns.toStream.filter(_.wasActive).sortBy { c =>
      (!c.wasActive, !c.wasPredicted, c.oldOverlap, c.ordinal)
    }.flatMap(_.cells.filter(_.active))
    //}.map(_.learningCell).filter(_.active)
  }

  def getLearningMotorNodes = {
    motorInput.segments.toStream.filter(_.wasActive).sortBy(_.activationOrdinal).reverse
  }

  def getInput: CLA.Input = {
    (for {
      column <- columns.iterator
      cell <- column.cells
    } yield cell.active).toIndexedSeq
  }

  //TODO: need to try inhibit columns that dont transition somewhere maybe....
  /*def inhibitColumns(): Unit = {
    val sorted = columns.sortBy(c => (!c.active, !c.feedForwardActive, -c.activeCount, -c.overlap, c.ordinal))

    sorted.drop(config.desiredLocalActivity).foreach(_.activeCount = 0)
  }*/
}

trait L3Layer[L] extends SequenceLayer {
  override type ColumnType = L3Column[L]
  type Location = L

  implicit val config: CLA.Config[L]

  def inhibitionRadius: Double

  //TODO: multiple learning cells when bursting?
  def getLearningNodes: Stream[NeuralNode] = {
    columns.toStream.filter(_.wasActive).sortBy { c =>
      (!c.wasActive, !c.wasPredicted, c.oldOverlap, c.ordinal)
    }.flatMap(_.cells.filter(_.active))
    //}.map(_.learningCell).filter(_.active)
  }
}