package com.colingodsey.logos.akka

import java.net.URLEncoder

import akka.actor._

import scala.concurrent.duration._

import com.colingodsey.logos.akka.BridgeProtocol.KillForeign

object BridgeActor {
  case class SendOut(foreignRef: String, senderRef: ActorRef, msg: Any)
  case class LocalDeathRelay(foreignRef: String)
}

//parent translates refs
class BridgeActor(foreignRef: String) extends Actor with ActorLogging {
  import BridgeActor._

  log.debug("bridge starting for " + foreignRef)

  def receive = {
    case x if sender != context.parent =>
      context.parent ! SendOut(foreignRef, sender, x)
  }

  override def postStop(): Unit = {
    super.postStop()

    context.parent ! LocalDeathRelay(foreignRef)

    log.debug("bridge stopping for " + foreignRef)
  }
}

//created for each foreign host
trait BridgeParent extends Actor with ActorLogging {
  import context.dispatcher

  def resolveStringToActorRef(str: String): ActorRef
  def resolveActorRefToString(ref: ActorRef): String
  def encodeName(str: String): String
  val transport: ActorRef

  private var foreignRefBridges = Map[String, ActorRef]()
  private var _remoteEndpoint: Option[ActorRef] = None

  private val tickTimer = context.system.scheduler.schedule(1.second, 5.seconds) {
    transport ! BridgeProtocol.Tick
  }

  def remoteEndpoint = _remoteEndpoint

  def getBridgeActorForForeignRef(foreignRef: String): ActorRef =
    foreignRefBridges get foreignRef match {
      case Some(x) => x
      case None =>
        val safeName = encodeName(foreignRef)
        val ref = context.actorOf(Props(new BridgeActor(foreignRef)), name = safeName)

        foreignRefBridges += foreignRef -> ref

        transport ! BridgeProtocol.Watch(foreignRef)

        ref
    }

  abstract override def preStart(): Unit = {
    super.preStart()

    context watch transport

    transport ! identity
  }

  abstract override def postStop(): Unit = {
    super.postStop()

    tickTimer.cancel()
  }

  def identify(msg: Any, ref: ActorRef): Unit = {
    val refStr = resolveActorRefToString(ref)
    transport ! BridgeProtocol.Identify(msg, refStr)
  }

  def identity = {
    val refStr = resolveActorRefToString(self)
    BridgeProtocol.Identify(BridgeProtocol.SessionEndpoint, refStr)
  }

  def bridgeReceive: Actor.Receive = {
    case BridgeProtocol.Message(msg, fromStr, toStr) =>
      val to = resolveStringToActorRef(toStr)
      val from = getBridgeActorForForeignRef(fromStr)

      to.tell(msg, from)
    case BridgeProtocol.ForeignDeath(fref) =>
      foreignRefBridges.get(fref) foreach context.stop
    case BridgeProtocol.Watch(fref) =>
      val ref = resolveStringToActorRef(fref)

      context watch ref
    case BridgeProtocol.Identify(msg, fromStr) =>
      val from = getBridgeActorForForeignRef(fromStr)

      if(msg == BridgeProtocol.SessionEndpoint)
        _remoteEndpoint = Some(from)

      self.tell(msg, from)

    case BridgeActor.SendOut(foreignRef, realSender, msg) =>
      //if we're getting this from one of our bridge actors, we must
      //redirect the message across the transport to the real dest

      val fromRef = resolveActorRefToString(realSender)

      transport ! BridgeProtocol.Message(msg, fromRef, foreignRef)

    case BridgeActor.LocalDeathRelay(fref) =>
      transport ! KillForeign(fref)

    case BridgeProtocol.Tick =>
      log.debug("tick")
      transport ! BridgeProtocol.Tock
    case BridgeProtocol.Tock =>
      log.debug("tock")

    case Terminated(`transport`) =>
      log.error("our transport has died!")
      context stop self

    case Terminated(ref) =>
      transport ! BridgeProtocol.ForeignDeath(resolveActorRefToString(ref))
  }
}
