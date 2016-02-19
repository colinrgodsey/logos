package akka.actor

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

trait Scheduler {
  val system: ActorSystem

  def shutdown(): Unit

  def schedule(
      initialDelay: FiniteDuration,
      interval: FiniteDuration,
      receiver: ActorRef,
      message: Any)(implicit executor: ExecutionContext,
      sender: ActorRef): Cancellable =
    schedule(initialDelay, interval) {
      import system._

      receiver ! message
      //if (receiver.isTerminated) sys.error(s"timer active for terminated actor " + receiver)

    }

  def schedule(
      initialDelay: FiniteDuration,
      interval: FiniteDuration)(f: ⇒ Unit)(
      implicit executor: ExecutionContext): Cancellable =
    schedule(initialDelay, interval, new Runnable { def run() = f })

  def schedule(
      initialDelay: FiniteDuration,
      interval: FiniteDuration,
      runnable: Runnable)(implicit executor: ExecutionContext): Cancellable

  def scheduleOnce(
      delay: FiniteDuration,
      receiver: ActorRef,
      message: Any)(implicit executor: ExecutionContext,
      sender: ActorRef): Cancellable =
    scheduleOnce(delay) {
      receiver ! message
    }

  def scheduleOnce(delay: FiniteDuration)(f: ⇒ Unit)(
      implicit executor: ExecutionContext): Cancellable =
    scheduleOnce(delay, new Runnable { def run() = f })

  def scheduleOnce(
      delay: FiniteDuration,
      runnable: Runnable)(implicit executor: ExecutionContext): Cancellable
}

trait Cancellable {
  def cancel(): Boolean
  def isCancelled: Boolean

  private val internalHashCode = math.random.toString.hashCode()

  override def hashCode(): Int = internalHashCode
}