package com.colingodsey.logos.cla.ui
/*
import com.colingodsey.logos.cla.server.Mailbox
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

trait BaseWebWorkerMain extends js.JSApp with Mailbox {
  import DOMExt._

  private implicit def ec = JSExecutionContext.queue

  object ParentRef extends Mailbox.BoxRef {
    def !(msg: Any): Unit =
      global.postMessage(Mailbox.Envelope(msg).toNativeJS)

    def ?(msg: Any)(implicit to: Mailbox.Timeout): Future[Any] = {
      val (id, fut) = newAskPromise

      global.postMessage(Mailbox.Envelope(msg, id).toNativeJS)

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
      worker.postMessage(Envelope(msg).toNativeJS)

    def ?(msg: Any)(implicit to: Timeout): Future[Any] = {
      val (id, fut) = newAskPromise

      worker.postMessage(Envelope(msg, id).toNativeJS)

      fut
    }
  }
}*/
