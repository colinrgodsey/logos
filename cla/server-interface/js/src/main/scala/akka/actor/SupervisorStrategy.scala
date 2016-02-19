package akka.actor

case class ChildRestartStats(child: ActorRef,
    maxNrOfRetriesCount: Int = 0,
    restartTimeWindowStartNanos: Long = 0L)

object SupervisorStrategy {
  sealed trait Directive

  type Decider = PartialFunction[Throwable, Directive]
}

trait SupervisorStrategy {
  import SupervisorStrategy._

  def decider: Decider
  def handleChildTerminated(context: ActorContext, child: ActorRef, children: Iterable[ActorRef]): Unit

  def processFailure(context: ActorContext, restart: Boolean, child: ActorRef,
      cause: Throwable, stats: ChildRestartStats,
      children: Iterable[ChildRestartStats])
}
