package com.colingodsey.logos.cla.ui

import akka.actor._
import akka.actor.pattern._
import com.colingodsey.logos.akka.WebSocketBridge
import com.colingodsey.logos.cla.server.ServerCommands
import com.colingodsey.logos.cla.server.ServerCommands.{GetStats, RunStats, Layer, ColumnView}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportDescendentObjects, JSExport}

import js.Dynamic.global

import org.scalajs.dom.ext.{Color => DOMColor}
import org.scalajs.{dom => window}

import org.scalajs.jquery.{JQueryStatic, JQuery}

import scala.concurrent.duration._

import DOMExt._

object KeyPressActor {
  case class KeyPress(code: Int, shiftKey: Boolean)

  case object Subscribe
  case object Unsubscribe
}

class KeyPressActor extends Actor {
  import KeyPressActor._

  var subscribers = Set[ActorRef]()

  $(global.document).keypress { e: window.KeyboardEvent =>
    self ! KeyPress(e.keyCode, e.shiftKey)

    true
  }

  def receive = {
    case x: KeyPress =>
      subscribers.foreach(_ ! x)
    case Subscribe =>
      context watch sender
      subscribers += sender
    case Unsubscribe =>
      context unwatch sender
      subscribers -= sender
    case Terminated(ref) =>
      subscribers -= ref
  }
}



class Guardian(props: Props) extends Actor with ActorLogging {
  import context.dispatcher

  var child = context.actorOf(props, name = "inst")

  context watch child

  object Restart

  def receive = {
    case Terminated(x) if x == child =>
      log.warning("restarting child")

      context.system.scheduler.scheduleOnce(3.5.seconds, self, Restart)
    case Restart =>
      child = context.actorOf(props, name = "inst")
      context watch child
  }
}

@JSExportDescendentObjects
trait CLAGame {
  @JSExport
  def stop(): Unit

}


