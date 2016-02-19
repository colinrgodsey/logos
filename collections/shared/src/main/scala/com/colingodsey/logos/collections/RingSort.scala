package com.colingodsey.logos.collections

import com.colingodsey.logos.macros.{RingSort => RingSortMacro}

import language.experimental.macros
import scala.reflect.ClassTag

object RingSort {
  private def takeTop_raw[T](numExpr: Int, valuesExpr: TraversableOnce[T]): Iterator[T] = macro RingSortMacro.takeTop_impl[T]

  final class LocalNullOrdering[T <: AnyRef](originalOrdering: Ordering[T]) extends Ordering[T] {
    def compare(x: T, y: T): Int = {
      if(x == null) -1
      else if(y == null) 1
      else originalOrdering.compare(x, y)
    }
  }

  class AnyRefSorter[T <: AnyRef: ClassTag](originalOrdering: Ordering[T]) extends RingSort[T] {
    def takeTop(num: Int, values: TraversableOnce[T]): Iterator[T] = {
      import Ordering.Implicits._

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
}

sealed trait RingSort[T] {
  def takeTop(num: Int, values: TraversableOnce[T]): Iterator[T]
}

