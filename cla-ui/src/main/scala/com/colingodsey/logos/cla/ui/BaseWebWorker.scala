package com.colingodsey.logos.cla.ui

import json._
import json.tools.AccessorRegistry

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import org.scalajs.{dom => window}

import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExport}

import js.Dynamic.global

object Mailbox {
  val registry = AccessorRegistry

  trait BoxRef {
    def !(msg: Any): Unit
    def ?(msg: Any)(implicit to: Timeout): Future[Any]

    def ask(msg: Any)(implicit to: Timeout): Future[Any] = `?`(msg)
  }

  case class Envelope(msg: Any, id: Option[String]) {
    def toJSON = this.js(Envelope.acc).toJSON
  }
  object Envelope {
    implicit val acc = {
      import registry.anyAccessor

      ObjectAccessor.of[Envelope]
    }

    def apply(msg: Any, id: String): Envelope =
      Envelope(msg, Some(id))

    def apply(msg: Any): Envelope =
      Envelope(msg, None)
  }

  case class Response(msg: Any, id: String) {
    def toJSON = this.js(Response.acc).toJSON
  }
  object Response {
    implicit val acc = {
      import registry.anyAccessor

      ObjectAccessor.of[Response]
    }
  }

  case class Timeout(to: FiniteDuration)

  def register(): Unit = {
    registry.addAccessor[Envelope]
    registry.addAccessor[Response]
  }

  register()
}

trait Mailbox {
  import Mailbox._

  private var idCounter = 1
  private var responseMap = Map.empty[String, Promise[Any]]

  private implicit def ec = JSExecutionContext.queue

  def receive: PartialFunction[Any, Any]

  protected def newAskPromise(implicit to: Timeout) = {
    val prom = Promise[Any]()
    val id = idCounter.toString
    var timerIdOpt: Option[Scheduler.Cancelable] = None

    idCounter += 1

    responseMap += id -> prom

    timerIdOpt = Some(Scheduler.scheduleOnce(to.to) {
      prom tryComplete Try {
        sys.error("Timeout failed after " + to.to)
      }
      timerIdOpt.foreach(_.cancel())
      timerIdOpt = None
    })

    prom.future.onComplete { _ =>
      timerIdOpt.foreach(_.cancel())
      timerIdOpt = None
      responseMap -= id
    }

    (id, prom.future)
  }

  protected def process(realMsg: Any, sender: BoxRef): Unit = realMsg match {
    case Envelope(Response(msg, id), _) =>
      responseMap(id).success(msg)
    case Envelope(msg, _) if !receive.isDefinedAt(msg) => //cant handle
    case Envelope(msg, None) => receive(msg)
    case Envelope(msg, Some(id)) =>
      receive(msg) match {
        case x: Future[_] => x.onComplete {
          case Failure(t) => throw t
          case Success(x) =>
            sender ! Response(x, id)
        }
        case x =>
          sender ! Response(x, id)
      }

  }
}


trait BaseWebWorkerMain extends js.JSApp with Mailbox {
  import DOMExt._

  private implicit def ec = JSExecutionContext.queue

  object ParentRef extends Mailbox.BoxRef {
    def !(msg: Any): Unit =
      global.postMessage(Mailbox.Envelope(msg).toJSON)

    def ?(msg: Any)(implicit to: Mailbox.Timeout): Future[Any] = {
      val (id, fut) = newAskPromise

      global.postMessage(Mailbox.Envelope(msg, id).toJSON)

      fut
    }
  }

  @JSExport
  def main(): Unit = {
    import Mailbox._

    println("worker started!")

    window.onmessage = { e: window.MessageEvent =>
      val env = JValue.from(e.data).to[Envelope]

      process(env, ParentRef)
    }
  }
}


trait BaseWebWorker extends Mailbox {
  import Mailbox._

  var responseMap = Map[String, Promise[js.Dynamic]]()

  val worker = new window.Worker("worker.js")

  worker.onmessage = { e0: js.Any =>
    val e = e0.asInstanceOf[js.Dynamic]
    val env = JValue.from(e.data).to[Envelope]

    process(env, WorkerRef)
  }

  object WorkerRef extends BoxRef {
    def !(msg: Any): Unit =
      worker.postMessage(Envelope(msg).toJSON)

    def ?(msg: Any)(implicit to: Timeout): Future[Any] = {
      val (id, fut) = newAskPromise

      worker.postMessage(Envelope(msg, id).toJSON)

      fut
    }
  }
}
