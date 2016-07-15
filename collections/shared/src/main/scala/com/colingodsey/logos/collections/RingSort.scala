package com.colingodsey.logos.collections

import com.colingodsey.logos.macros.{RingSort => RingSortMacro}

import language.experimental.macros

import scala.reflect.ClassTag

import Ordering.Implicits._

//TODO: replace with useful @specializedNoOrdered annotation
object RingSort {
  final class LocalNullOrdering[T](originalOrdering: Ordering[T]) extends Ordering[T] {
    def compare(x: T, y: T): Int = {
      if(x == null) -1
      else if(y == null) 1
      else originalOrdering.compare(x, y)
    }
  }

  private def takeTop_raw[T](numExpr: Int, valuesExpr: TraversableOnce[T]): Iterator[T] = macro RingSortMacro.takeTop_impl[T]

  class AnyRefSorter[T <: AnyRef: ClassTag](originalOrdering: Ordering[T]) extends RingSort[T] {
    def takeTop(num: Int, values: TraversableOnce[T]): Iterator[T] = {
      implicit val nullOverrideOrdering = new LocalNullOrdering(originalOrdering)

      val ring = new Array[T](num)

      takeTop_raw[T](num, values)
    }
  }
  
  implicit def anyRefSorter[T <: AnyRef: Ordering: ClassTag]: RingSort[T] = new AnyRefSorter[T](implicitly[Ordering[T]])

  implicit val DoubleSorter = new RingSort[Double] {
    def takeTop(num: Int, values: TraversableOnce[Double]): Iterator[Double] = {
      val ring = new Array[Double](num)

      takeTop_raw[Double](num, values)
    }
  }

  implicit val IntSorter = new RingSort[Int] {
    def takeTop(num: Int, values: TraversableOnce[Int]): Iterator[Int] = {
      val ring = new Array[Int](num)

      takeTop_raw[Int](num, values)
    }
  }

  def takeTop[T: RingSort](num: Int, values: TraversableOnce[T]): Iterator[T] =
    implicitly[RingSort[T]].takeTop(num, values)

  @inline private def translateIndex(idx: Int, ringOffset: Int, num: Int): Int = {
    val x = idx + ringOffset

    if(x >= num) x - num
    else x
  }

  //implicit def anyRefSorter[@specialized T: Ordering: ClassTag]: RingSort[T] = new TakeTopGeneric[T](implicitly[Ordering[T]])

  class TakeTopGeneric[@specialized T: ClassTag](ordering: Ordering[T]) extends RingSort[T] {
    implicit val nullOverrideOrdering = new LocalNullOrdering(ordering)

    def takeTop(num: Int, values: TraversableOnce[T]) = {
      var ringOffset = 0

      var total = 0
      val itr = values.toIterator

      val ring = new Array[T](num)

      while (itr.hasNext) {
        val value = itr.next()

        val first = ring(translateIndex(0, ringOffset, num))

        if (value > first) {
          //largest in ring, bump idx back
          ringOffset -= 1

          if (ringOffset < 0) ringOffset += num

          ring(translateIndex(0, ringOffset, num)) = value
        } else if (value > ring(translateIndex(num - 1, ringOffset, num))) {
          //in range but not first
          var currentValue = value
          var offs = 0

          while (offs < num) {
            val valAtOffs = ring(translateIndex(offs, ringOffset, num))

            //if greater, bump back.
            if (currentValue > valAtOffs) {
              //found a value we're larger than.
              //replace old value and add old back in
              ring(translateIndex(offs, ringOffset, num)) = currentValue
              //switch in old value, keep going down the line
              currentValue = valAtOffs //may recur up to 'num' depth
            } //else continue

            offs += 1
          }
        } //else ignore

        total += 1
      }

      Iterator.from(0) map { idx =>
        ring(translateIndex(idx, ringOffset, num))
      } take math.min(total, num)
    }
  }
}

sealed trait RingSort[T] {
  def takeTop(num: Int, values: TraversableOnce[T]): Iterator[T]
}

