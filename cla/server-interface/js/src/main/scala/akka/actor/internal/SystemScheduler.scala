package akka.actor.internal

import akka.actor.{ActorSystem, Cancellable, Scheduler}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.Dynamic._
import scala.util.control.NonFatal

//TODO: attach actor that can 'watch' actor targets
class SystemScheduler(val system: ActorSystem) extends Scheduler {
  //This set seems to be really slow??
  //NOTE: regular objects and primitives lack hashcode...
  private val timers = mutable.Set[Cancellable]()

  class TimeoutCancel(delay: FiniteDuration,
      runnable: Runnable)(
      implicit executor: ExecutionContext) extends Cancellable {
    def lambda: js.Function = () => if(isRunning) {
      //system.log.debug(s"$timerId fired")
      cancel()

      try runnable.run() catch {
        case NonFatal(t) => executor.reportFailure(t)
      }
    }

    private var timerId: Option[js.Dynamic] = Some(
      global.setTimeout(lambda, delay.toMillis))

    //system.log.debug(s"new timer id $timerId ${delay.toMillis}")

    def cancel(): Boolean = {
      val wasRunning = isRunning

      timers -= this

      for(id <- timerId) global.clearTimeout(id)
      timerId = None

      wasRunning
    }

    def isRunning = timerId != None
    def isCancelled = !isRunning
  }

  class IntervalCancel(initialDelay: FiniteDuration,
      interval: FiniteDuration,
      runnable: Runnable)(
      implicit executor: ExecutionContext) extends Cancellable {

    //TODO: make use real executor
    def lambda: js.Function = () => if(isRunning) {
      try {
        runnable.run()

        if(!hasInited && isRunning) {
          timerId = Some(global.setInterval(lambda, interval.toMillis))
          system.log.debug(s"$timerId interval start ${interval.toMillis}")
          hasInited = true
        }
      } catch {
        case NonFatal(t) =>
          executor.reportFailure(t)
          cancel()
      }
    }

    private var timerId: Option[js.Dynamic] = Some(
      global.setTimeout(lambda, initialDelay.toMillis))
    private var hasInited = false

    def isRunning = timerId != None
    def isCancelled: Boolean = !isRunning

    def cancel(): Boolean = {
      val wasRunning = isRunning

      timers -= this

      timerId match {
        case Some(x) if !hasInited => //still the timeout
          global.clearTimeout(x)
        case Some(x) => //now interval finally
          global.clearInterval(x)
        case None =>
      }

      timerId = None

      wasRunning
    }
  }

  def shutdown(): Unit = {
    for(timer <- timers) timer.cancel()
    timers.clear()
  }

  def schedule(initialDelay: FiniteDuration,
      interval: FiniteDuration,
      runnable: Runnable)(
      implicit executor: ExecutionContext): Cancellable = {
    val c = new IntervalCancel(initialDelay, interval, runnable)

    timers += c

    c
  }

  def scheduleOnce(delay: FiniteDuration,
      runnable: Runnable)(
      implicit executor: ExecutionContext): Cancellable = {
    val c = new TimeoutCancel(delay, runnable)

    timers += c

    c
  }
}
