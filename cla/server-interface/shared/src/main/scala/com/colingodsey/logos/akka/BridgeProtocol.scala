package com.colingodsey.logos.akka

import json._
import json.tools.AccessorRegistry

sealed trait BridgeProtocol
object BridgeProtocol {
  //from foreign to local. receivers local is remotes foreign and reverse
  case class Message(msg: Any, fromRef: String, toRef: String) extends BridgeProtocol
  object Message {
    import AccessorRegistry.anyAccessor

    implicit val acc = ObjectAccessor.of[Message]
  }

  case class Watch(foreignRef: String) extends BridgeProtocol
  object Watch {
    implicit val acc = ObjectAccessor.of[Watch]
  }

  case class ForeignDeath(foreignRef: String) extends BridgeProtocol
  object ForeignDeath {
    implicit val acc = ObjectAccessor.of[ForeignDeath]
  }

  case class KillForeign(foreignRef: String) extends BridgeProtocol
  object KillForeign {
    implicit val acc = ObjectAccessor.of[KillForeign]
  }

  //send a message to the remote session actor itself.
  case class Identify(msg: Any, foreignRef: String) extends BridgeProtocol
  object Identify {
    import AccessorRegistry.anyAccessor

    implicit val acc = ObjectAccessor.of[Identify]
  }

  case object Tick extends BridgeProtocol
  case object Tock extends BridgeProtocol

  case object SessionEndpoint

  object registry extends AccessorRegistry

  registry.add[Message]
  registry.add[Watch]
  registry.add[ForeignDeath]
  registry.add[KillForeign]
  registry.add[Identify]
  registry.add(SessionEndpoint)
  registry.add(Tick)
  registry.add(Tock)

  AccessorRegistry.add(registry)

  //define this last so we dont confuse our internal implicits
  implicit def bridgeAcc = registry.anyAccessor.asInstanceOf[ObjectAccessor[BridgeProtocol]]
}

trait BridgeProtocolObjects {
  
}