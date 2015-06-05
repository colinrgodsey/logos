package com.colingodsey.logos.cla.shadow

import java.util.concurrent.Executors

import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.duration._

object VMImpl {
  def distributedExec[T](chunkSize: Int, items: IndexedSeq[T])(f: T => Unit)(implicit ec: ExecutionContext): Unit = {
    val vectorBuilder = new VectorBuilder[Future[Unit]]
    var i = 0
    var firstChunk: Iterator[T] = null

    while(i < items.length) {
      val chunk = items.iterator.slice(i, i + chunkSize)

      if(firstChunk == null) firstChunk = chunk
      else vectorBuilder += Future(chunk foreach f)

      i += chunkSize
    }

    val futures = vectorBuilder.result()
    val future = Future.sequence(futures)

    //reuse same thread for first chunk
    if(firstChunk != null) firstChunk foreach f

    Await.result(future, 100.seconds)

    ()
  }

  def newDefaultExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)
}
