package akka.actor

import akka.actor.internal.Cell

import scala.scalajs.js.annotation.{JSExportAll, JSExportDescendentObjects}

object Actor {
  type Receive = PartialFunction[Any, Unit]

  //ala akka
  private[actor] var _activeCell: Option[Cell] = None

  //def createUserRef(): ActorRef = ???

  private def fetchCell(): Cell = {
    require(_activeCell != None, "ActiveCell state issue - b")

    val cell = _activeCell.get
    _activeCell = None

    cell
  }
}

@JSExportDescendentObjects
@JSExportAll
trait Actor {
  import akka.actor.Actor._

  type Receive = Actor.Receive

  private final val cell: Cell = {
    val c = fetchCell()
    c.actor = this
    c
  }

  final val context: ActorContext = cell
  implicit final val self: ActorRef = cell.self

  final def sender = cell.sender

  //behavior stack
  @inline private def realReceive: Receive =
    if(cell.receiveStack.isEmpty) receive
    else cell.receiveStack.head

  def receive: Receive

  def preStart() {}
  def postStop() {}

  private[actor] def internalReceive: Any => Unit = realReceive orElse {
    case PoisonPill => context stop self
    case Terminated(ref) =>
      this match {
        case x: ActorLogging =>
          x.log.error("Death pact exception from " + ref)
        case _ =>
          context.system.log.error(s"[$self] Death pact exception from " + ref)
      }
      context stop self
    case x => //unhandled
      context.system.log.debug(s"unhandled $x for ref $self")
  }

  private[actor] final def start() = {
    preStart()
  }
  private[actor] final def stop() = {
    postStop()
  }
}