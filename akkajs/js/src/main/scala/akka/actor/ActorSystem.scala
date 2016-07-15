package akka.actor

import java.net.URI
import java.util.logging.Level

import akka.actor.internal.{Cell, SystemScheduler}
import akka.event.LoggingAdapter
import akka.pattern.AskTimeoutException
import akka.util.Timeout

import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.scalajs.js
import scala.util.control.NonFatal

import org.scalajs.{dom => window}

object ActorSystem {
  def apply(name0: String): ActorSystem = new ActorSystem {
    val name = name0
  }

  private[actor] def newId: String = newIntId.toString
  private[actor] def newIntId: Int =
    math.random.toString().hashCode()
}

//0 debug, 1 info, 2 warn, 3 error
class JSConsoleLoggingAdapater(minLevel: Int = 1) extends LoggingAdapter {
  def isErrorEnabled: Boolean = minLevel <= 3
  def isWarningEnabled: Boolean = minLevel <= 2
  def isInfoEnabled: Boolean = minLevel <= 1
  def isDebugEnabled: Boolean = minLevel <= 0

  def error(msg: => String): Unit =
    if(isErrorEnabled) window.console.error(msg)

  def error(cause: Throwable, msg: => String): Unit = if(isErrorEnabled) {
    window.console.error(cause.asInstanceOf[js.Any])
    window.console.error(msg)
    cause.printStackTrace()
  }

  def warning(msg: => String): Unit = if(isWarningEnabled) window.console.warn(msg)

  def warning(cause: Throwable, msg: => String): Unit = if(isWarningEnabled) {
    window.console.warn(cause.asInstanceOf[js.Any])
    window.console.warn(msg)
    cause.printStackTrace()
  }

  def info(msg: => String): Unit = if(isInfoEnabled) window.console.info(msg)

  def debug(msg: => String): Unit = if(isDebugEnabled) window.console.info(msg)
}

trait ActorSystem { thisSystem =>

  def name: String

  val log: LoggingAdapter = new JSConsoleLoggingAdapater()
  val scheduler = new SystemScheduler(this)

  lazy val address = Address("akka", name, "localhost", 0)
  lazy val rootPath = RootActorPath(address)
  lazy val userPath = rootPath / "user"

  private var _isRunning = true
  private var actors = Map[ActorPath, LocalActorRef]()

  def addActor(ref: LocalActorRef): Unit = {
    require(!(actors contains ref.path), "path already exists for " + ref)

    actors += ref.path -> ref
  }

  def remActor(ref: LocalActorRef): Unit = {
    require(hasActor(ref), "cannot remove actor thats not in the system!")

    actors -= ref.path
  }

  def hasActor(ref: ActorRef): Boolean = actors.get(ref.path) == Some(ref)

  def isRunning = _isRunning

  //private[actor] def isTerminated(ref: ActorRef): Boolean = !(actors contains ref)

  def stop(ref: ActorRef): Unit = ref.stop()

  def shutdown(): Unit = {
    _isRunning = false
    trap(scheduler.shutdown())
    for(actor <- actors) trap(actor._2.stop())
  }

  private def trap(f: => Unit): Unit = {
    try f catch {
      case NonFatal(t) =>
        log.error(t, "trapper error")
    }
  }

  def resolveActorRef(str: String): ActorRef = try {
    val uri = new URI(str)
    val uid = uri.getFragment.toInt
    val path = ActorPath(str)

    def deadRef = newDeadRef(path, uid)

    actors get path match {
      case Some(x) if x.path.uid == uid => x
      case None => deadRef
    }
  } catch {
    case NonFatal(t) =>
      log.error(t, "Failed to parse actor ref string")
      deadLetters
  }

  private[actor] def newDeadRef(_path: ActorPath, _uid: Int): ActorRef = new LocalActorRef {
    val path: ActorPath = _path

    private[actor] var _cell: Cell = null
    val uid: Int = _uid
    private[actor] val system: ActorSystem = thisSystem
  }

  def actorOf(props: Props, name: String = ""): ActorRef = new LocalActorRef { ref =>
    val id = if(name == "") ActorSystem.newId else name
    val system = thisSystem

    private[actor] var _cell: Cell = new SystemCell(this, props)

    val path = (userPath / id).withUid(_cell.uid)

    if(actors contains path)
      sys.error(s"Actor $ref already exists!")

    ref.cell.init()
  }

  private[actor] class SystemCell(val self: LocalActorRef,
      val props: Props, parentOpt: Option[ActorRef] = None) extends Cell {
    val system = ActorSystem.this
    val parent = parentOpt getOrElse deadLetters
    implicit val dispatcher = system.dispatcher
  }

  def ask(ref: ActorRef, msg: Any)(implicit to: Timeout): Future[Any] = {
    val tempRef = new TempActorRef(msg, ref, to)

    tempRef.future
  }

  class TempActorRef private[actor](msg: Any, dest: ActorRef,
      to: Timeout) extends LocalActorRef { tempActor =>
    val name = "temp" + ActorSystem.newId
    val path = rootPath / name
    val system = thisSystem

    private val promise = Promise[Any]

    def future = promise.future

    private val props = Props(new TempActor(msg, dest, to, promise))
    private[actor] var _cell: Cell = new SystemCell(this, props)

    cell.init()
  }

  implicit object dispatcher extends ExecutionContext {
    def execute(runnable: Runnable) = {
      //require(isRunning, "not running")
      if(!isRunning)
        log.warning("denying execution because of shutdown")
      else js.Dynamic.global.setTimeout({() =>
        try { if(isRunning) runnable.run() }
        catch { case t: Throwable => reportFailure(t) }
      }, 0)
    }

    def reportFailure(t: Throwable): Unit = {
      log.error(t, "Uncaught dispatcher exception")
    }
  }

  lazy val deadLetters: ActorRef = new LocalActorRef { _deadLetters =>
    val path = rootPath / "dead-letters"
    val system = thisSystem

    private[actor] var _cell: Cell = new Cell {
      val self = _deadLetters
      val system = thisSystem
      implicit val dispatcher = system.dispatcher
      def parent = self

      override private[actor] def tell(msg: Any, from: ActorRef): Unit =
        if(from != _deadLetters && isRunning) super.tell(msg, from)

      def props: Props = Props(new Actor {
        def receive: Receive = {
          case x =>
            log.warning(s"deadLetters received $x from $sender")
        }
      })
    }

    cell.init()
  }
}

class TempActor private[actor](msg: Any, dest: ActorRef,
    to: Timeout, promise: Promise[Any]) extends Actor {
  import context.dispatcher
  val TimeIsUp = math.random

  val timer = context.system.scheduler.scheduleOnce(to.duration, self, TimeIsUp)

  def isTargetTerminated = dest match {
    case x: LocalActorRef => x.isTerminated
    case x => false
  }

  override def preStart(): Unit = {
    super.preStart()

    //TODO: check dest.isTerminated

    if(isTargetTerminated) {
      self ! Terminated(dest)
    } else {
      dest ! msg
      context watch dest
    }
  }

  override def postStop(): Unit = {
    super.postStop()

    promise.tryFailure(AskTimeoutException(s"temp actor somehow died before resolution"))
  }

  def receive: Receive = {
    case Terminated(`dest`) =>
      promise.tryFailure(AskTimeoutException(s"actor $dest has terminated"))
      context stop self
      timer.cancel()
    case TimeIsUp if !promise.isCompleted =>
      promise.tryFailure(AskTimeoutException(s"ask timed out"))
      context stop self
    case x if !promise.isCompleted =>
      promise.trySuccess(x)
      context stop self
      timer.cancel()
  }
}
