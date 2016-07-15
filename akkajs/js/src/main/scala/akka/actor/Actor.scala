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

  def receive: Receive

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

  protected[akka] def aroundReceive(receive: Actor.Receive, msg: Any): Unit = receive.applyOrElse(msg, unhandled)

  /**
    * Can be overridden to intercept calls to `preStart`. Calls `preStart` by default.
    */
  protected[akka] def aroundPreStart(): Unit = preStart()

  /**
    * Can be overridden to intercept calls to `postStop`. Calls `postStop` by default.
    */
  protected[akka] def aroundPostStop(): Unit = postStop()

  /**
    * Can be overridden to intercept calls to `preRestart`. Calls `preRestart` by default.
    */
  protected[akka] def aroundPreRestart(reason: Throwable, message: Option[Any]): Unit = preRestart(reason, message)

  /**
    * Can be overridden to intercept calls to `postRestart`. Calls `postRestart` by default.
    */
  protected[akka] def aroundPostRestart(reason: Throwable): Unit = postRestart(reason)

  def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    context.children foreach { child ⇒
      context.unwatch(child)
      context.stop(child)
    }
    postStop()
  }
  def preStart(): Unit = {}
  def postRestart(reason: Throwable): Unit = {
    preStart()
  }
  def postStop(): Unit = {}

  private[actor] def internalReceive: Receive = realReceive orElse {
    case PoisonPill => context stop self
    case x => unhandled(x)
  }

  def unhandled(msg: Any): Unit = msg match {
    case Terminated(ref) =>
      this match {
        case x: ActorLogging =>
          x.log.error("Death pact exception from " + ref)
        case _ =>
          context.system.log.error(s"[$self] Death pact exception from " + ref)
      }

      throw DeathPactException(ref)
    case x =>
      context.system.log.debug(s"unhandled $x for ref $self")
  }

}