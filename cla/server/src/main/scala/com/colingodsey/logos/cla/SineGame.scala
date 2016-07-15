package com.colingodsey.logos.cla

import akka.actor._
import com.colingodsey.logos.cla.encoders.ScalarEncoder
import com.colingodsey.logos.cla.server.ServerCommands
import json._
import json.tools.AccessorRegistry

import scala.collection.{mutable, IndexedSeqOptimized}
import scala.concurrent.blocking
import scala.concurrent.duration._

import scala.concurrent.Future
import scala.concurrent.duration.Deadline

class SineGame(uiRef: ActorRef, width: Int) extends NeuralEntityActor with BaseGame {
  import ServerCommands.SineGame._

  implicit val config = CLA.ReducedConfig.copy(regionWidth = width)
  import config._
  import topology.Location

  case object RunSome

  val pointsBuffer = 3000

  val encoder = new ScalarEncoder(config.inputWidth, config.minOverlap + 5)
  val region = new FullRegion
  //val region = new L3Region
  val started = Deadline.now

  val thalamusSource: CLA.InputSource = region.l5Layer
  val thalamusSDR = new InputSDR[Location](thalamusSource)(config.copy(regionWidth = config.inputWidth))
  val thalamus: ActorRef = context.actorOf(ThalamusActor(inputWidth, inputWidth), name = "thalamus")
  var thalamusOutput: Option[CLA.Input] = Some(new Array[Boolean](inputWidth))

  var totalPointsReceived = 0
  var totalPointsAskedFor = 0
  var runRequested = false

  var currentPointBuffer: List[Double] = Nil


  def numCurrentPoints: Int = currentPointBuffer.length
  def numPendingPoints = totalPointsAskedFor - totalPointsReceived
  def pointsNeeded = pointsBuffer - numCurrentPoints
  def pointsToAskFor = pointsNeeded - numPendingPoints

  def maybeRunSome() = if(!runRequested && autoRunEnabled && numCurrentPoints > 0 && canTick) {
    runRequested = true
    self ! RunSome
  }

  def checkPoints(): Unit = {
    val n = pointsToAskFor

    if(n > 0) {
      val ask = math.max(n, pointsBuffer)
      log.debug(s"asking for $ask points")
      uiRef ! NeedDataPoints(ask)
      totalPointsAskedFor += ask
    }
  }

  def canTick = thalamusOutput != None

  def runTick(point: Double): Unit = if(canTick) blocking {
    region.update(encoder.encode(point), thalamusOutput.get)
    thalamusOutput = None

    val reward = if(-started.timeLeft < 5.seconds && point < 0.1) 1.0 else 0.0

    thalamusSDR.update(thalamusSource.produce)
    //thalamus ! ThalamusActor.ReceiveSensoryInput(encoder.encode(point))
    thalamus ! ThalamusActor.ReceiveInput(thalamusSDR.produce)
    thalamus ! ThalamusActor.Update(reward)

    updateStats()

    ticks += 1
  } else log.warning("missing external input!")

  def runSome(): Unit = if(currentPointBuffer.nonEmpty) {
    val point = currentPointBuffer.head

    runTick(point)

    currentPointBuffer = currentPointBuffer.tail
    checkPoints()
  } else checkPoints()

  override def preStart(): Unit = {
    super.preStart()

    log.info("sine game started")

    uiRef ! ServerCommands.GameIdentity
    context watch uiRef

    thalamus ! NeuralEntityActor.Subscribe
    context watch thalamus

    checkPoints()
  }

  def resume(): Unit = maybeRunSome()

  def receive = neuralEntityActorReceive orElse baseGameReceive orElse {
    case DataPoints(points) =>
      currentPointBuffer = currentPointBuffer ++ points
      totalPointsReceived += points.length
      maybeRunSome()
      checkPoints()
    case ClearPoints =>
      currentPointBuffer = Nil
      checkPoints()
    case RunSome =>
      runRequested = false

      runSome()

      if(numCurrentPoints > 0) maybeRunSome()
    case RunOne => runSome()

    case ThalamusActor.ThalamusOutput(input) =>
      require(input.length == inputWidth, s"expected width $inputWidth, got ${input.length} ")
      thalamusOutput = Some(input)
      maybeRunSome()
  }
}
