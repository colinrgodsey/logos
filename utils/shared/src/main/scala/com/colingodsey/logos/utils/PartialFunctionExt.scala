package com.colingodsey.logos.utils

trait PartialFunctionExt {
  import scala.{PartialFunction => PF}

  implicit class PartialFunctionExtension[T, U](val pf: PF[T, U]) {
    def ++(other: PF[T, U])(
        implicit ct: PartialFunctionExtension.ConcatType[U]) = new PF[T, U] {
      def isDefinedAt(x: T): Boolean = pf.isDefinedAt(x) || other.isDefinedAt(x)

      def apply(arg: T): U = {
        val aOpt = pf.lift(arg)
        val bOpt = other.lift(arg)

        (aOpt, bOpt) match {
          case (Some(x), Some(y)) => ct.concat(x, y)
          case (None, Some(x)) => x
          case (Some(x), None) => x
          case (None, None) => throw new MatchError(arg)
        }
      }
    }
  }

  object PartialFunctionExtension {
    object ConcatType {
      implicit object UnitConcatType extends PartialFunctionExtension.ConcatType[Unit] {
        def concat(a: Unit, b: Unit): Unit = {}
      }

      implicit def inlineConcatType[T](implicit f: (T, T) => T): ConcatType[T] = new ConcatType[T] {
        def concat(a: T, b: T): T = f(a, b)
      }
    }
    trait ConcatType[T] {
      def concat(a: T, b: T): T
    }
  }


}
