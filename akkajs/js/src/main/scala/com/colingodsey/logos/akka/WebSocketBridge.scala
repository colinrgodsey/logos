package com.colingodsey.logos.akka

import akka.actor._
import json._
import org.scalajs.dom.WebSocket
import org.scalajs.{dom => window}

import scalajs.js

object WebSocketBridge {
  private case object Connected

  case class Identify(msg: Any)
}

//"ws://localhost:7131/"
class WebSocketBridge(path: String) extends Actor with ActorLogging with Stash {
  import WebSocketBridge._

  val ws = new WebSocket(path)//, "soap")

  val session = context.actorOf(Props(new WebSocketSession), name = "session")

  override def preStart(): Unit = {
    super.preStart()

    initWSListeners()

    context watch session
  }

  override def postStop(): Unit = {
    super.postStop()

    log.info("closing websocket")

    ws.close()
  }

  def initWSListeners(): Unit = {
    ws.onopen = { e: window.Event =>
      log.info("Websocket open on " + path)
      self ! Connected
    }
    ws.onerror = { e: window.ErrorEvent =>
      val ex = js.JavaScriptException(e)

      log.error(ex, "WebSocket error encountered on " + path)
      self ! PoisonPill
    }
    ws.onmessage = { e: window.MessageEvent =>
      (e.data: Any) match {
        case str: String =>
          log.debug("received " + str)

          val msg = JValue.fromString(str).toObject[BridgeProtocol]
          session ! msg
        case x =>
          log.error("Expected str, got " + x)
          self ! PoisonPill
      }
    }
    ws.onclose = { e: window.CloseEvent =>
      log.info("websocket closed")
      self ! PoisonPill
    }
  }

  def receive: Receive = {
    case PoisonPill => context stop self
    case Connected =>
      log.info("ws connected!")
      context become normalReceive
      unstashAll()
    case _ => stash()
  }

  def normalReceive: Receive = {
    case x: Identify => session.tell(x, sender)
    case x: BridgeProtocol if sender == session =>
      //val str = (x.js: JValue).toDenseString
      val str = js.JSON.stringify(x.js.asInstanceOf[js.Any])

      ws.send(str)
      log.debug("sending " + str)
  }
}

class WebSocketSession extends Actor with ActorLogging with BridgeParent {
  val transport: ActorRef = context.parent

  def receive = bridgeReceive orElse {
    case WebSocketBridge.Identify(msg) =>
      log.debug(s"sending identity $msg from $sender")
      identify(msg, sender)
  }

  def resolveStringToActorRef(str: String): ActorRef = context.system.resolveActorRef(str)

  def resolveActorRefToString(ref: ActorRef): String = ref.toString

  def encodeName(str: String): String = str
}
