package akka

import akka.actor._

import scala.concurrent.{ExecutionContext, Future}

import scala.language.implicitConversions
import scala.util.{Success, Failure}

package object pattern {
  case class AskTimeoutException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

  class PipeableFuture[T](val x: Future[T]) extends AnyVal {
    def pipeTo(ref: ActorRef)(implicit ec: ExecutionContext): Unit = {
      x.onComplete {
        case Success(x) =>
          ref ! x
        case Failure(t) =>
          ref ! Status.Failure(t)
      }
    }
  }

  implicit def pipe[T](future: Future[T])(implicit executionContext: ExecutionContext): PipeableFuture[T] = new PipeableFuture(future)
}
