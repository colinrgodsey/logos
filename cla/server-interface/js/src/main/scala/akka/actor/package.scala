package akka.actor

import java.net.URI

import akka.actor.internal.{PropsMaker, Cell}
import akka.event.LoggingAdapter

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.language.experimental.macros

case class Terminated(ref: ActorRef)

case class Timeout(dur: FiniteDuration)

trait ActorLogging { _: Actor =>
  lazy val log = new ActorLoggingAdapter(self, context.system)
}

class ActorLoggingAdapter(ref: ActorRef, system: ActorSystem) extends LoggingAdapter {
  def prefix = s"[${ref.toString}] "

  def isErrorEnabled: Boolean = system.log.isErrorEnabled
  def isInfoEnabled: Boolean = system.log.isInfoEnabled
  def isWarningEnabled: Boolean = system.log.isWarningEnabled
  def isDebugEnabled: Boolean = system.log.isDebugEnabled

  def warning(msg: => String): Unit = system.log.warning(prefix + msg)
  def warning(cause: Throwable, msg: => String): Unit = system.log.warning(cause, prefix + msg)
  def error(msg: => String): Unit = system.log.error(prefix + msg)
  def error(cause: Throwable, msg: => String): Unit = system.log.error(cause, prefix + msg)
  def debug(msg: => String): Unit = system.log.debug(prefix + msg)
  def info(msg: => String): Unit = system.log.info(prefix + msg)
}

trait ActorContext {
  val self: ActorRef
  val system: ActorSystem
  implicit val dispatcher: ExecutionContext

  def parent: ActorRef
  def sender: ActorRef
  def children: Iterable[ActorRef]

  def watch(watchee: ActorRef): Unit
  def unwatch(watchee: ActorRef): Unit

  def stop(other: ActorRef): Unit
  def actorOf(props: Props, name: String = ""): ActorRef

  def become(receive: Actor.Receive, discardOld: Boolean = true): Unit
  def unbecome(): Unit
}

case class Address(protocol: String, system: String, host: String, port: Int) {
  override lazy val toString = s"$protocol://$system@$host:$port"
}

final case object PoisonPill

//TODO: log to deadletters on shutdown
trait Stash { _: Actor =>
  private type Stashed = (ActorRef, Any)

  private val theStash = mutable.Queue[Stashed]()

  private def localSelf = self.asInstanceOf[LocalActorRef]
  private def msg = localSelf.cell._msg

  def stash(): Unit = {
    theStash enqueue sender -> msg
  }

  def unstashAll(): Unit = {
    while(theStash.nonEmpty) {
      val (s, m) = theStash.dequeue()

      self.tell(m, s)
    }
  }
}

trait ActorPath {
  def name: String
  def parent: ActorPath
  def root: RootActorPath
  def address: Address
  def reversePath: List[String]

  def safeName = URIEncoding.encode(name)

  def /(child: String): ActorPath = ChildActorPath(this, child)

  def path = reversePath.reverse

  //override val toString: String

  final override lazy val hashCode: Int = toString.hashCode
}

object ActorPath {
  def apply(str: String): ActorPath = {
    val uri = new URI(str)
    val addr = Address(uri.getScheme, uri.getUserInfo, uri.getHost, uri.getPort)
    val pathSeq = uri.getPath.split("/").iterator.filter(_ != "").map(URIEncoding.decode)
    val uid = uri.getFragment.toInt

    pathSeq.foldLeft(RootActorPath(addr): ActorPath)(_ / _)
  }
}

final case class ChildActorPath private[akka](parent: ActorPath, name: String) extends ActorPath {
  lazy val root: RootActorPath = parent.root
  lazy val address: Address = root.address

  def reversePath: List[String] = name +: parent.reversePath

  override lazy val toString = s"$parent/$safeName"
}

final case class RootActorPath private[akka](address: Address) extends ActorPath {
  def root: RootActorPath = this
  def parent: ActorPath = this

  def name: String = "/"

  def reversePath: List[String] = Nil

  override lazy val toString = address.toString
}

object Status {
  sealed trait Status
  case class Failure(cause: Throwable) extends Status
  case class Success(status: AnyRef) extends Status
}

object ActorRef {
  private[actor] final val noSender: ActorRef = null //ala akka
}

trait ActorRef extends Equals {
  def path: ActorPath

  def tell(msg: Any, from: ActorRef): Unit

  private[actor] def stop(): Unit
  private[actor] def addWatcher(watcher: ActorRef): Unit
  private[actor] def removeWatcher(watcher: ActorRef): Unit

  def !(msg: Any)(implicit sender: ActorRef = ActorRef.noSender) = tell(msg, sender)
  def ?(msg: Any)(implicit to: Timeout): Future[Any]

  def canEqual(that: Any): Boolean = that.isInstanceOf[ActorRef]

  override def equals(that: Any): Boolean = that match {
    case r: ActorRef => r.toString == toString
    case _ => false
  }

  final override lazy val hashCode: Int = toString.hashCode
}

private[actor] trait LocalActorRef extends ActorRef { self =>
  private[actor] var _cell: Cell
  private[actor] def system: ActorSystem
  private[actor] def uid: Int

  private[actor] def cell: Cell = _cell

  private[actor] def isTerminated: Boolean = cell match {
    case null => true
    case x => !x.isRunning
  }

  private[actor] def addWatcher(watcher: ActorRef): Unit =
    if(cell != null) cell.addWatcher(watcher)
  private[actor] def removeWatcher(watcher: ActorRef): Unit =
    if(cell != null) cell.removeWatcher(watcher)

  def ?(msg: Any)(implicit to: Timeout): Future[Any] =
    system.ask(self, msg)

  def tell(msg: Any, from: ActorRef): Unit =
    if(cell != null) cell.tell(msg, from)
    else {
      //system.deadLetters ! msg
      system.log.warning(s"msg $msg undelivered to dead ref $self")
    }

  private[actor] def stop(): Unit =
    if(cell != null) cell.stop()

  final override lazy val toString = s"$path#${uid}"
}

object URIEncoding {
  def encode(str: String): String = {
    val length = str.length

    var finished = false
    var start = 0
    var i = 0

    val buffer = new StringBuilder(length, "")

    @inline def curChar = str.charAt(i)
    @inline def isSafe(c: Char): Boolean =
      ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.'
          || c == '*')

    while(!finished) {
      while (i < length && isSafe(curChar)) i += 1
      buffer ++= str.substring(start, i)

      if(i >= length) finished = true
      else if(curChar == ' ') {
        buffer += '+'
        i += 1
      } else {
        start = i

        def notSafe: Boolean = {
          val c = curChar
          c != ' ' && !isSafe(c)
        }

        while(i < length && notSafe) i += 1
        val unsafe = str.substring(start, i)
        val bytes = unsafe.getBytes("UTF-8")

        for(b <- bytes) {
          val x: Int = b

          buffer += '%'
          buffer += hex.charAt((x & 0xf0) >> 4)
          buffer += hex.charAt(x & 0x0f)
        }

      }

      start = i
    }

    buffer.result()
  }

  def decode(str: String): String = {
    val length = str.length

    var i = 0
    var start = 0
    var finished = false

    val buffer = new StringBuilder(length, "")

    @inline def curChar = str.charAt(i)
    @inline def isEncoded(c: Char): Boolean = c == '%' || c == '+'

    while(!finished) {
      while (i < length && !isEncoded(curChar)) i += 1
      buffer ++= str.substring(start, i)

      if(i >= length) finished = true
      else if(curChar == '+') {
        buffer += ' '
        i += 1
      } else {
        val byteBuilder = Array.newBuilder[Byte]
        while(i < length && curChar == '%') {
          val s = str.substring(i + 1, i + 3)
          byteBuilder += java.lang.Byte.parseByte(s, 16)
          i += 3
        }

        val bytes = byteBuilder.result()

        buffer ++= new String(bytes, "UTF-8")
      }

      start = i
    }

    buffer.result()
  }

  private final val hex = "0123456789ABCDEF"
}
