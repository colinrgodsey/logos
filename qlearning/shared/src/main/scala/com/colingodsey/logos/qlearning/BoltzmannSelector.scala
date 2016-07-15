package com.colingodsey.logos.qlearning

import scala.annotation.tailrec

object BoltzmannSelector {
  val default = apply()

  final def weightedSegmentSelect[K](chances0: TraversableOnce[(K, Double)]): Stream[K] = {
    val chances = chances0.toStream
    val seed = math.random
    val shuffled = chances.sortBy(x => (x, seed).hashCode)

    var theSum = chances.iterator.map(_._2).sum
    var selected = Set[K]()
    var lastGood: (K, Double) = chances.head

    //TODO: redo each time where skip each selected key and dont add to acc, remove from sub, pick new key

    @tailrec def iter(acc: Double, theList: Iterator[(K, Double)], selection: Double): K = {
      val head = theList.next()
      val key = head._1
      val entryValue = head._2
      val max = acc + entryValue

      val isSelected = selected(key)

      if(!isSelected) lastGood = head

      if(isSelected && theList.hasNext) { //already selected
        iter(acc, theList, selection)
      } else if (isSelected && !theList.hasNext) { //end of the line
        theSum -= lastGood._2
        selected += lastGood._1
        lastGood._1
      } else if (selection <= max) { //inside range
        theSum -= entryValue
        selected += key
        key
      } else {
        iter(max, theList, selection)
      }
    }

    //TODO: make this only reset iteration of new selection is less than previos
    val selectionItr = Iterator continually {
      lastGood = chances.head
      iter(0.0, shuffled.iterator, theSum * math.random)
    }

    selectionItr.toStream.distinct.take(chances.length)
  }
}

case class BoltzmannSelector(temperature: Double = 10) extends Selector {
   def selectFrom[T](policy: TraversableOnce[(T, Double)]): T = {
     streamFrom(policy).next()
   }

  def streamFrom[T](policy: TraversableOnce[(T, Double)]): Iterator[T] = {
    require(temperature > 0.0)

    val pl = policy.toStream
    var max = Double.MinValue
    var min = Double.MaxValue

    pl.foreach { pair =>
      val x = pair._2
      if(x < min) min = x
      if(x > max) max = x
    }

    val d = max - min

    if(d < 0.0001) { //just randomly pick one if they're this close in value
      val seed = math.random

      pl.sortBy(x => x._1.hashCode() * seed).iterator.map(_._1)
    } else {
      val invD = 1.0 / d
      val distribution = pl map { case (key, x) =>
        val v = math.exp((x - min) * invD * temperature)
        (key, v)
      }

      (BoltzmannSelector weightedSegmentSelect distribution).iterator
    }

  }
 }
