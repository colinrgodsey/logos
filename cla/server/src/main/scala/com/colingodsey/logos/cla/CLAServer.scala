package com.colingodsey.logos.cla

import java.net.{URLEncoder, InetSocketAddress}

import akka.actor._
import akka.pattern._
import akka.io.IO
import akka.serialization.Serialization
import com.colingodsey.logos.akka.{BridgeProtocol, BridgeParent}
import com.colingodsey.logos.cla.server.ServerCommands
import json._
import json.tools.AccessorRegistry

import spray.can.Http
import spray.can.server.UHttp
import spray.can.websocket
import spray.can.websocket.frame.{Frame, BinaryFrame, TextFrame}
import spray.http.HttpRequest
import spray.can.websocket.FrameCommandFailed
import spray.routing.HttpServiceActor


object CLAServer extends App {
  val system = ActorSystem("cla-server")

  ServerCommands.register()

  system.actorOf(Props[CLAServer], name = "cla-server")

  println("press enter to close")

  while (System.in.read() != '\n') {}

  println("closing server")
  system.shutdown()
}

class CLAServer extends Actor with ActorLogging {
  import context.system

  IO(UHttp) ! Http.Bind(self, "localhost", 7131)

  def receive = {
    case Http.Bound =>
      log.info("port bound!")
    case Http.Connected(remoteAddress, localAddress) =>
      log.info("new connection")
      val conn = context.actorOf(Props(classOf[SessionActor], sender))
      sender ! Http.Register(conn)
  }
}

class SessionActor(val serverConnection: ActorRef)
    extends HttpServiceActor with ActorLogging with websocket.WebSocketServerWorker {
  import context.dispatcher

  val serverSession = context.actorOf(Props[ClientSession], name = "session")

  override def receive = handshaking orElse defaultRoute orElse closeLogic

  def businessLogic: Receive = ({
    case websocket.UpgradedToWebSocket =>
      log.debug("Websocket established")

    case x: BridgeProtocol if sender == serverSession =>
      val str = x.js.toDenseString

      log.debug("sending data " + str)
      send(TextFrame(str))

    case TextFrame(payload) => //inbound
      val str = payload.utf8String
      val x = JValue.fromString(str).toObject[BridgeProtocol]

      serverSession ! x
      log.debug("incoming data " + str)

    case _: BinaryFrame =>
      sys.error("no binary support")

    case x: FrameCommandFailed =>
      log.error("frame command failed", x)

    case ev: Http.ConnectionClosed =>
      context.stop(self)
      log.info("connection has closed")
  }: Receive) orElse defaultRoute

  def defaultRoute: Receive = {
    implicit val refFactory: ActorRefFactory = context
    runRoute {
      getFromResourceDirectory("webapp")
    }
  }

  override def postStop(): Unit = {
    log.info("terminating session")
    super.postStop()
  }
}

class ClientSession extends BridgeParent {
  val transport = context.parent

  def extendedSystem = context.system.asInstanceOf[ExtendedActorSystem]
  def resolveStringToActorRef(str: String): ActorRef =
    extendedSystem.provider.resolveActorRef(str)
  def resolveActorRefToString(ref: ActorRef): String =
    Serialization.serializedActorPath(ref)

  def encodeName(str: String): String = URLEncoder.encode(str, "UTF8")

  def receive = bridgeReceive orElse {
    case ServerCommands.SineGame.Start(regionWidth) =>
      val props = Props(classOf[SineGame], sender, regionWidth)

      context.actorOf(props)

    case ServerCommands.GoalGame.Start(regionWidth) =>
      val props = Props(classOf[GoalGame], sender, regionWidth)

      context.actorOf(props)
  }
}












