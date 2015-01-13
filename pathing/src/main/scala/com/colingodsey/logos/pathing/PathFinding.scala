package com.colingodsey.logos.pathing

import scala.concurrent.blocking

trait PathFinding[State <: Equals, Move <: Equals] {
	type Paths = Stream[(State, List[Move])]

	def maxPathLength: Int

	def legalNeighbors(state: State): Stream[(State, Move)]

	def neighborsWithHistory(b: State, history: List[Move]): Paths = {
		if(history.length > maxPathLength) Stream()
		else legalNeighbors(b).toStream map {
			case (block, move) =>
				(block, move :: history)
		}
	}

	def newNeighborsOnly(neighbors: Paths,
			explored: Set[State]): Paths = neighbors.headOption match {
		case None => Stream()
		case Some((state, moves)) if !explored(state) =>
			Stream(state -> moves) #::: newNeighborsOnly(neighbors.tail, explored + state)
		case _ => newNeighborsOnly(neighbors.tail, explored)
	}

	def pathsFrom(initial0: Paths, explored0: Set[State]): Paths = {
		//var explored = Set[State](initial0.head._1)
		var explored = explored0

		def moreFrom(state: State, moves: List[Move]): Paths = for {
			nextPath @ (nextState, nextMoves) <- {
				val ns = neighborsWithHistory(state, moves).filter(x => !explored(x._1))

				explored ++= ns.map(_._1)

				ns
			}
			next <- Stream(nextPath) append moreFrom(nextState, nextMoves)
		} yield next

		initial0 #::: moreFrom(initial0.head._1, initial0.head._2)
	}

	def pathsFrom(start: State): Paths =
		pathsFrom(Stream((start, Nil)), Set(start))

	def infinitePathsFrom(start: State, to: State): Paths = {
		val fromStart = pathsFrom(start)

		fromStart.filter {
			case (state, _) => state == to
		}
	}

	def pathFromCond(start: State, to: State, of: Int = 1000)(
      cond: State => Boolean): Option[Seq[Move]] = blocking {
    if(start == to) Some(Nil)
    else infinitePathsFrom(start, to).take(of).filter(
      pair => cond(pair._1)).headOption.map(_._2.reverse)
	}

  def pathFrom(start: State, to: State, of: Int = 1000): Option[Seq[Move]] =
    pathFromCond(start, to, of)(x => x == to)
}
