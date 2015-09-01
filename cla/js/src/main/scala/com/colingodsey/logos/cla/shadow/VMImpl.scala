package com.colingodsey.logos.cla.shadow

import scala.concurrent.ExecutionContext

import scala.scalajs.concurrent.JSExecutionContext

object VMImpl {
   def distributedExec[T](chunkSize: Int, items: IndexedSeq[T])(f: T => Unit): Unit =
    items foreach f

   def newDefaultExecutionContext: ExecutionContext = JSExecutionContext.queue
 }
