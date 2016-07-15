package akka.actor.internal

import akka.actor.Props

import language.experimental.macros
import scala.reflect.ClassTag

/** Dummy object to get the right shadowing for 2.10 / 2.11 cross compilation */
private object Compat210 {
  object blackbox {
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._

object PropsMaker {
  import scala.reflect.macros._ // shadows blackbox from above
  import blackbox.Context

  def newProps[T](clazz: Class[T], args: AnyRef*) = macro newProps_impl[T]

  def newPropsNoArg_impl[T: c.WeakTypeTag](c: Context): c.Expr[Props] = {
    import c.universe._

    val typ = implicitly[c.WeakTypeTag[T]].tpe

    val classTagExpr = TypeTree(typ)

    val tree = Apply(
      Select(
        New(TypeTree(typ)),
        termNames.CONSTRUCTOR
      ),
      Nil
    )

    val newInst = c.Expr[AnyRef](tree)

    val maker = reify {
      (() => newInst.splice)
    }

    val resTree = q""" new akka.actor.Props.PropsImpl(classOf[$classTagExpr])($maker) """

    c.Expr[Props](resTree)
  }

  def newProps_impl[T: c.WeakTypeTag](c: Context)(clazz: c.Expr[Class[T]],
      args: c.Expr[AnyRef]*): c.Expr[Props] = {
    import c.universe._

    val typ = implicitly[c.WeakTypeTag[T]].tpe

    val tree = Apply(
      Select(
        New(TypeTree(typ)),
        termNames.CONSTRUCTOR
      ),
      args.map(_.tree).toList
    )

    val argTree = Apply(
      Select(
        Ident(TermName("Seq")),
        TermName("apply")
      ),
      args.map(_.tree).toList
    )

    val newInst = c.Expr[AnyRef](tree)

    val maker = reify {
      (() => newInst.splice)
    }

    val resTree = q""" new akka.actor.Props.PropsImpl($clazz, $argTree: _*)($maker) """

    c.Expr[Props](resTree)
  }
}
