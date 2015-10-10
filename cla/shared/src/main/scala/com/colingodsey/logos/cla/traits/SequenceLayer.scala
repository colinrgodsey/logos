package com.colingodsey.logos.cla.traits

import com.colingodsey.logos.cla.{Column, Cell, Layer, NeuralNode}

trait TypedSequenceLayer[L] extends SequenceLayer {
  type Location = L
}

trait SequenceLayer extends Layer {
  import com.colingodsey.logos.cla.CLA._

  type Location

  override type ColumnType = Column[Location]

  implicit val config: Config[Location]

  def getLearningNodes: Stream[NeuralNode]
  //def input(idx: Int): Boolean
  //def inhibitionRadius: Double

  def numActiveFromPrediction = columns.count(column => column.active && column.wasPredicted)
  def numActive = columns.count(_.active)

  def anomalyScore = {
    val na = numActive.toDouble

    if(na == 0) 0.0
    else 1.0 - numActiveFromPrediction / na
  }

  def getLearningNodes(c: LearningColumn): Stream[NeuralNode]  =
    getLearningNodes.filter {
      case cell: Cell => cell.column != c
      case _ => true
    }

  def activeColumns = columns.iterator.filter(_.active)

  //this is always what 'was' active
  def getLearningCells: Stream[Cell] = {
    columns.toStream.filter(_.wasActive).sortBy { c =>
      (!c.wasActive, /*!c.wasPredicted, */c.oldOverlap, c.ordinal)
    }.flatMap { column =>
      val activeCells = column.cells.filter(_.active)

      if(activeCells.length == column.cells.length)
        Seq(column.learningCell)
      else activeCells
    }
  }

  def getCurrentLearningCells: Stream[Cell] = {
    columns.toStream.filter(_.active).sortBy { c =>
      (!c.active, c.overlap, c.ordinal)
    }.flatMap { column =>
      val activeCells = column.cells.filter(_.active)

      if(activeCells.length == column.cells.length)
        Seq(column.learningCell)
      else activeCells
    }
  }
}
