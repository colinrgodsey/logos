package akka.actor

import scala.concurrent.{ExecutionContext, Future}

package object pattern {
  case class AskTimeoutException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

  implicit class PipeAdder(val x: Future[Any]) extends AnyVal {
    def pipeTo(ref: ActorRef)(implicit ec: ExecutionContext): Unit = {
      x.onComplete {
        case util.Success(x) =>
          ref ! x
        case util.Failure(t) =>
          ref ! Status.Failure(t)
      }
    }
  }
}
