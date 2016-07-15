package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.{TypedSequenceLayer, SequenceLayer}

trait ExternalLearningLayer[L] extends TypedSequenceLayer[L] {
  implicit val config: CLA.Config[L]

  def requiresExtraLayerNodes = false

  //TODO: mix cells, and high activation min? or.... what?
  def getLearningNodes: Stream[NeuralNode] = {
    val extraCellsStream = extraLayerLearningNodes

    val itr = new Iterator[NeuralNode] {
      val columnCells = getLearningCells.iterator
      val extraCells = extraCellsStream.iterator

      var isOdd = false

      def hasNext: Boolean = columnCells.hasNext || extraCells.hasNext

      def next(): NeuralNode = {
        isOdd = !isOdd

        (columnCells.hasNext, extraCells.hasNext) match {
          case (true, true) if isOdd => columnCells.next()
          case (true, true) => extraCells.next()
          case (true, false) => columnCells.next()
          case (false, true) => extraCells.next()
          case (false, false) => sys.error("No more elements!")
        }
      }
    }

    if(requiresExtraLayerNodes && extraCellsStream.length <
        (config.segmentThreshold * 3.0 / 4.0)) Stream.empty
    else itr.toStream
  }

  def extraLayerLearningNodes: Stream[NeuralNode]
}

trait InternalLearningLayer[L] extends TypedSequenceLayer[L] {
  implicit val config: CLA.Config[L]

  def getLearningNodes: Stream[NeuralNode] = getLearningCells
}