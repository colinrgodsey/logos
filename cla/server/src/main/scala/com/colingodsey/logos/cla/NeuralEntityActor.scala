package com.colingodsey.logos.cla

import akka.actor._

object NeuralEntityActor {
  case object Subscribe
  case object Unsubscribe
}

trait NeuralEntityActor extends Actor with ActorLogging {
  import NeuralEntityActor._

  private var subscribers = Set[ActorRef]()

  def publish(msg: Any) = subscribers.foreach(_ ! msg)

  def unsubscribe(ref: ActorRef): Unit = {
    subscribers -= ref
    context unwatch ref
  }

  def subscribe(ref: ActorRef): Unit = {
    subscribers += ref
    context watch ref
  }

  def neuralEntityActorReceive: Receive = {
    case Subscribe => subscribe(sender)
    case Unsubscribe => unsubscribe(sender)
    case Terminated(ref) if subscribers(ref) =>
      subscribers -= ref
  }
}