package com.colingodsey.logos.macros

import scala.annotation.{StaticAnnotation, tailrec}

import language.experimental.macros
//import scala.reflect.NameTransformer.encode


/** Dummy object to get the right shadowing for 2.10 / 2.11 cross compilation */
private object Compat210 {
  @deprecated("", "")
  object whitebox {
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._
object RingSort {
  import scala.reflect.macros._ // shadows blackbox from above
  import whitebox._

  def takeTop_impl[T: c.WeakTypeTag](c: Context)(numExpr: c.Expr[Int], valuesExpr: c.Expr[TraversableOnce[T]]): c.Expr[Iterator[T]] = {
    @deprecated("", "")
    object Shadow210 {
      def TermName(s: String) = c.universe.newTermName(s)

      def TypeName(s: String) = c.universe.newTypeName(s)

      val termNames = c.universe.nme

      def noSelfType = c.universe.emptyValDef

      def pendingSuperCall = {
        import c.universe._

        Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)
      }

      implicit class SymbolExt(s: c.universe.ClassSymbol) {
        def companion = s.companionSymbol
      }

      implicit class MethodSymbolExt(s: c.universe.MethodSymbol) {
        def paramLists = s.paramss
      }

      implicit class AnnotationExt(a: c.universe.Annotation) {
        object tree {
          def tpe = a.tpe

          object children {
            def tail = a.scalaArgs
          }
        }
      }
    }

    import Shadow210._
    {
      import c.universe._

      val ringExpr = c.Expr[Array[T]](Ident(TermName("ring")))

      //inlined tree that is used in several places
      val translateIndexTree =
        q"""
        {
          val x = __idx + ringOffset
          if(x >= num) x - num
          else x
        }
      """

      //set __idx when using tree. force inlined
      val translateIndexExpr = c.Expr[Int](translateIndexTree)

      //using quasi-quotes here to keep the typing loose until added to a tree
      val GT1Expr = c.Expr[Boolean]( q""" value > first  """)
      val GT2Expr = c.Expr[Boolean]( q""" value > { val __idx = num - 1; $ringExpr($translateIndexTree) } """)
      val GT3Expr = c.Expr[Boolean]( q""" currentValue > valAtOffs """)

      //single solid block of sorting code
      reify {
        val num = numExpr.splice
        val values = valuesExpr.splice

        val itr = values.toIterator

        var ringOffset = 0
        var total = 0

        while (itr.hasNext) {
          val value = itr.next()

          val first = ringExpr.splice {
            val __idx = 0
            translateIndexExpr.splice
          }

          if (GT1Expr.splice) {
            //largest in ring, bump idx back
            ringOffset -= 1

            if (ringOffset < 0) ringOffset += num

            val __idx = 0
            ringExpr.splice(translateIndexExpr.splice) = value
          } else if (GT2Expr.splice) {
            //in range but not first
            var currentValue = value
            var offs = 0

            while (offs < num) {
              val valAtOffs = ringExpr.splice {
                val __idx = offs
                translateIndexExpr.splice
              }

              //if greater, bump back.
              if (GT3Expr.splice) {
                //found a value we're larger than.
                //replace old value and add old back in
                val __idx = offs
                ringExpr.splice(translateIndexExpr.splice) = currentValue
                //switch in old value, keep going down the line
                currentValue = valAtOffs //may recur up to 'num' depth
              } //else continue

              offs += 1
            }
          } //else ignore

          total += 1
        }

        Iterator.from(0) map { __idx =>
          ringExpr.splice(translateIndexExpr.splice)
        } take math.min(total, num)
      }
    }
  }
}