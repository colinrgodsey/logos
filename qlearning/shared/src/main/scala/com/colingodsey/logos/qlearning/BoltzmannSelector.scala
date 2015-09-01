package com.colingodsey.logos.qlearning

import scala.annotation.tailrec

object BoltzmannSelector {
  final def weightedSegmentSelect[K, V](chances0: Map[K, V])
      (implicit num: Numeric[V]): K = {

    val chances = chances0 map { case (k, v) =>
      k -> math.max(num toDouble v, 0)
    }
    val (keys0, values) = chances.unzip
    val rnd = math.random
    val keys = keys0.toSeq.sortBy(x => (x, rnd).hashCode)
    val sel = math.random * values.sum

    @tailrec def iter(acc: Double, keyList: Iterable[K]): K = {
      val key = keyList.head
      val entryValue = chances(key)
      val max = acc + entryValue

      //inside range
      if (sel <= max) key
      else iter(max, keyList.tail)
    }

    iter(0.0, keys)
  }

  lazy val default = apply()
}

case class BoltzmannSelector(temperature: Double = 0.1) extends Selector {
   def selectFrom[T](policy: Map[T, Double]): T = {
     require(temperature > 0.0)

     val pl = policy//.toSeq.sortBy(_ => math.random)
     val distribution = pl map { case (key, x) =>
         val v = math.exp((x - pl.head._2) / temperature)
         (key, v)
       }

     BoltzmannSelector weightedSegmentSelect distribution
   }

 }
