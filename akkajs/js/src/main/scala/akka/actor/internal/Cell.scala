package akka.actor.internal

import akka.actor._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

trait Cell extends ActorContext { thisCell =>
  def props: Props
  val self: LocalActorRef

  val throughput = 200

  private val messageQueue = mutable.Queue[(ActorRef, Any)]()
  private var watchers = Set[ActorRef]()
  private var watching = Set[ActorRef]()
  private var realWatching = Set[ActorRef]()

  private[actor] val uid: Int = ActorSystem.newIntId

  private[actor] var actor: Actor = null
  private[actor] var _msg: Any = null
  private var _sender: Option[ActorRef] = None
  private var _isRunning = false
  private var isScheduled = false
  private var childNames = Map[String, ActorRef]()
  private var _children: Set[ActorRef] = Set[ActorRef]()
  private[actor] var receiveStack: List[Actor.Receive] = Nil

  def children = _children

  def isRunning = _isRunning

  private def addChild(child: ActorRef): Unit = {
    if(children.contains(child) || childNames.contains(child.path.name))
      sys.error("name already exists!")

    _children += child
    childNames += child.path.name -> child
  }

  private def removeChild(child: ActorRef): Unit = {
    _children -= child
    childNames -= child.path.name
  }

  def sender: ActorRef = _sender match {
    case Some(x: ActorRef) => x
    case _ => system.deadLetters //null or None
  }

  def processQueue(): Unit = runSafe {
    var i = 0
    try while(messageQueue.nonEmpty && i < throughput && _isRunning) {
      val (s, msg) = messageQueue.dequeue()

      _sender = Some(s)
      _msg = msg

      try internalReceive(msg) catch {
        case NonFatal(t) =>
          tryRestart(t, Some(msg))
      }

      i += 1
    } finally {
      _sender = None
      _msg = null
    }

    if(i != 0) system.log.debug(s"Processed $i messages for $self")

    checkScheduled()
  }

  def internalReceive(msg: Any): Unit = msg match {
    //soft-watch wachees so we can clean up refs
    case Terminated(ref) if watching(ref) =>
      watchers -= ref
      watching -= ref
      if(children(ref)) { //the unfortunate!
        removeChild(ref)
        //TODO: supervision logic here
      }
      if(realWatching(ref)) { //forward to actor receive
        realWatching -= ref
        actor.internalReceive(msg)
      }
    case Terminated(ref) => //TODO: probably log this
    case _ => actor.internalReceive(msg)
  }

  def tellTerminated(): Unit = {
    for(watcher <- watchers) watcher ! Terminated(self)
    for(child <- children) system stop child
    _children = children.empty
    //watchers.clear()
    watchers = watchers.empty
    realWatching = realWatching.empty
  }

  private[actor] def addWatcher(watcher: ActorRef): Unit = watchers += watcher

  private[actor] def removeWatcher(watcher: ActorRef): Unit = watchers -= watcher

  def watch(watchee: ActorRef) = if(!realWatching(watchee)) {
    require(watchee != null, "how is this null?")

    watchee.addWatcher(self)
    watching += watchee
    realWatching += watchee
  }
  def unwatch(watchee: ActorRef) = if(realWatching(watchee)) {
    require(watchee != null, "how is this null?")

    watchee.removeWatcher(self)
    watching -= watchee
    realWatching -= watchee
  }

  def tryRestart(cause: Throwable, msg: Option[Any]) = if(_isRunning) {
    _isRunning = false

    actor.aroundPreRestart(cause, msg)

    system.log.error(cause, s"$self threw error, trying restart")

    //TODO: check supervisor policy here!!

    receiveStack = Nil

    init(Some(cause))

    actor.aroundPostRestart(cause)
  }

  def stop() = if(_isRunning) {
    _isRunning = false

    trap(watching.foreach(unwatch))
    trap(system.remActor(self))
    trap(tellTerminated())
    trap(actor.aroundPostStop())
    children.foreach(x => trap(x.stop()))

    self._cell = null
  }

  def stop(other: ActorRef): Unit = system stop other

  def become(receive: Actor.Receive, discardOld: Boolean = true): Unit = {
    if(discardOld/* && receiveStack.nonEmpty*/)
      receiveStack = receive +: Nil//receiveStack.tail
    else
      receiveStack = receive +: receiveStack
  }

  def unbecome(): Unit = {
    if(receiveStack.nonEmpty) receiveStack = receiveStack.tail
  }

  def actorOf(props: Props, name: String = ""): ActorRef = {
    val child = new LocalActorRef {
      val id = if(name == "") ActorSystem.newId else name
      val system = thisCell.system

      private[actor] var _cell: Cell =
        new thisCell.system.SystemCell(this, props, Some(thisCell.self))

      val path = (thisCell.self.path / id).withUid(_cell.uid)

      val uid: Int = _cell.uid
    }

    if(system hasActor child)
      sys.error(s"Actor $child already exists!")

    addChild(child)

    child.cell.init()

    //dont forget to watch the children!
    watching += child
    child.addWatcher(self)

    child
  }

  private def trap(l: => Unit): Unit = {
    try l catch {
      case t: Throwable =>
        system.log.error(t, "TRAP")
    }
  }

  private def runSafe(l: => Unit): Unit = try l catch {
    case NonFatal(t) =>
      system.log.error(t, s"Actor $self died with exception")
      trap(stop())
  }

  private[actor] def init(restart: Option[Throwable] = None): Unit = {
    import akka.actor.Actor._

    require(!_isRunning, "already running!")

    if(!restart.isDefined)
      system.addActor(self)

    _isRunning = true

    actor = null

    require(_activeCell == None, "ActiveCell state issue - a")
    _activeCell = Some(this)

    //inject cell
    try props.create() catch {
      case NonFatal(t) =>
        trap(system.remActor(self))
        _isRunning = false
        throw t
    } finally _activeCell = None

    require(_activeCell == None, "ActiveCell state issue - c")
    require(actor != null, "failed to set actor inst")

    restart match {
      case Some(t) =>
        runSafe(actor.aroundPostRestart(t))
      case None =>
        runSafe(actor.aroundPreStart())
    }

    system.log.debug("Created new actor " + self)
  }

  private[actor] def checkScheduled() =
    if(!isScheduled && messageQueue.nonEmpty && _isRunning) {
      isScheduled = true
      system.dispatcher.execute(new Runnable {
        def run(): Unit = {
          isScheduled = false
          if(_isRunning) processQueue()
        }
      })
    }

  private[actor] def tell(msg: Any, from: ActorRef): Unit = runSafe {
    if(_isRunning) {
      //system.log.debug(s"$self: rcv $msg from $from")
      messageQueue enqueue from -> msg
      checkScheduled()
    } else system.deadLetters.tell(msg, from)
  }
}