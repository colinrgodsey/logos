package akka.actor

import akka.actor.internal.PropsMaker

import scala.reflect.ClassTag

import scala.language.experimental.macros

//props here is super hacky because we need macros to create the 'reflect' newinstance abilities
object Props {
  def apply[T <: Actor](clazz: Class[T], args: AnyRef*): Props = macro PropsMaker.newProps_impl[T]
  def apply[T <: Actor]: Props = macro PropsMaker.newPropsNoArg_impl[T]
  def apply[T <: Actor](maker: => T): Props = DirectProps(() => maker)

  private[actor] case class DirectProps(maker: () => Actor) extends Props {
    private[actor] def create() = maker()
  }
  case class PropsImpl[T <: Actor](clz: Class[T], args: AnyRef*)(maker: () => T) extends Props {
    private[actor] def create() = maker()
  }
}
sealed trait Props {
  private [actor] def create()
}