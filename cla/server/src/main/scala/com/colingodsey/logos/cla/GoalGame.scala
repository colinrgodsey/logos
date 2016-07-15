package com.colingodsey.logos.cla

import akka.actor._
import com.colingodsey.logos.cla.CLA.InputSource
import com.colingodsey.logos.cla.encoders.ScalarEncoder
import com.colingodsey.logos.cla.server.ServerCommands
import com.colingodsey.logos.cla.server.ServerCommands.GoalGame.ActorPosition
import com.colingodsey.logos.collections.Vec2
import json._
import json.tools.AccessorRegistry

import scala.collection.{mutable, IndexedSeqOptimized}
import scala.concurrent.blocking
import scala.concurrent.duration._

import scala.concurrent.Future
import scala.concurrent.duration.Deadline

class GoalGame(uiRef: ActorRef, width: Int) extends NeuralEntityActor with BaseGame {
  import ServerCommands.GoalGame._
  import context.dispatcher

  implicit val config = CLA.ReducedConfig.copy(regionWidth = width)
  import config._
  import topology.Location

  case object SendPos
  case object DoTick

  val pointsBuffer = 3000
  val actorSpeed = 1.3
  val turnSpeed = 0.06
  val gameDimensions = 400
  val halfGameDimensions = gameDimensions / 2
  val rewardDist = 80
  val tooCloseDist = 26

  val region = new FullRegion {
    override def l5Updated(): Unit = GoalGame.this.l5Updated()
  }
  val posEncoder = new ScalarEncoder(config.inputWidth / 3 - 10, config.minOverlap * 2, -halfGameDimensions, halfGameDimensions)
  val rotEncoder = new ScalarEncoder(config.inputWidth / 3 - 10, config.minOverlap * 2, 0, 2 * math.Pi)

  val posTimer = context.system.scheduler.schedule(1.second, 30.millis, self, SendPos)

  val thalamusInputSource: CLA.InputSource = region.l5Layer
  val thalamus: ActorRef = context.actorOf(
    ThalamusActor(inputWidth, thalamusInputSource.width, inputWidth / 10),
    name = "thalamus")
  val thalamusOnly: ActorRef = context.actorOf(
    ThalamusActor(inputWidth, inputWidth, inputWidth / 10),
    name = "thalamus-only")
  var thalamusOnlyOutput: Option[CLA.Input] = Some(new Array[Boolean](inputWidth))
  var thalamusOutput: Option[CLA.Input] = Some(new Array[Boolean](inputWidth))
  var actorMoveDir = Vec2.zero
  var tactorMoveDir = Vec2.zero

  val thalamusOutSource = new InputSource {
    def width: Int = inputWidth

    def iterator: Iterator[Boolean] = thalamusOutput.get.iterator

    override def produce = thalamusOutput.get
  }

  /*val motorSource = MixInputs(MixInputs(thalamusOutSource, thalamusOutSource), thalamusInputSDR)
  val motorSDR = new InputSDR(motorSource)(config.copy(
    overlapPercent = config.overlapPercent / 3,
    regionWidth = config.inputWidth,
    dynamicInhibitionRadiusScale = config.dynamicInhibitionRadiusScale * 5,
    inputRangeSpreadPercent = config.inputRangeSpreadPercent * 3
  ))*/

  var tactorPos: Vec2 = Vec2.zero//random * halfGameDimensions
  var tactorRotation: Double = 0.0

  var actorPos: Vec2 = Vec2.zero//random * halfGameDimensions
  var actorRotation: Double = 0.0
  var tickRequested = false
  var isCorticalMotorControlEnabled = true

  var currentAward = 0.0

  def actorDirVec: Vec2 = Vec2(math.cos(actorRotation), math.sin(actorRotation))
  def tactorDirVec: Vec2 = Vec2(math.cos(tactorRotation), math.sin(tactorRotation))

  def distFromReward = (actorPos - Vec2.zero).length
  def tdistFromReward = (tactorPos - Vec2.zero).length
  def withinReward = distFromReward < rewardDist
  def twithinReward = tdistFromReward < rewardDist

  def maybeRunSome(force: Boolean = false) =
    if((autoRunEnabled || force) && canTick && !tickRequested) {
      self ! DoTick
      tickRequested = true
    }

  def canTick = thalamusOutput != None && thalamusOnlyOutput != None

  override def getInterestingInput = region.motorInput//motorSDR

  def l5Updated(): Unit = {

    thalamus ! ThalamusActor.ReceiveInput(thalamusInputSource.produce)
    //thalamus ! ThalamusActor.ReceiveSensoryInput(sensoryInput)
    thalamus ! ThalamusActor.Update(currentAward)

    //thalamusOnly ! ThalamusActor.ReceiveCorticalInput(...)
    //thalamusOnly ! ThalamusActor.ReceiveSensoryInput(tsensoryInput)
    //thalamusOnly ! ThalamusActor.Update(treward + tspeedReward)
  }

  def runTick(): Unit = if(canTick) blocking {
    actorPos = clampWorldPos(actorPos)
    tactorPos = clampWorldPos(tactorPos)

    val thalVec = actorPos - tactorPos

    /*if(thalVec.length < tooCloseDist) {
      actorPos += thalVec.safeNormal * 1
    }*/

    while(actorRotation < 0) actorRotation += math.Pi * 2
    while(actorRotation > math.Pi * 2) actorRotation -= math.Pi * 2

    while(tactorRotation < 0) tactorRotation += math.Pi * 2
    while(tactorRotation > math.Pi * 2) tactorRotation -= math.Pi * 2

    val pad = IndexedSeq.fill(10)(false)

    val sensoryInput =  pad ++
      posEncoder.encode(actorPos.x) ++ pad ++
      rotEncoder.encode(actorRotation) ++ pad ++
      posEncoder.encode(actorPos.y)

    val tsensoryInput =  pad ++
      posEncoder.encode(tactorPos.x) ++  pad ++
        rotEncoder.encode(tactorRotation) ++  pad ++
        posEncoder.encode(tactorPos.y)

    //val speedReward = (1.0 - math.abs(actorMoveDir.x)) * 0.001
    //val tspeedReward = (1.0 - math.abs(tactorMoveDir.x)) * 0.001

    //simple reward for having a low l5 anomaly
    val anomalyAward = 1.0 - region.l5Layer.anomalyScore

    val speedReward = -math.abs(actorMoveDir.x) * 0.0001
    val tspeedReward = -math.abs(tactorMoveDir.x) * 0.0001

    val half = halfGameDimensions
    val reward =
      if(withinReward) 1.0 - distFromReward / rewardDist.toDouble + 0.25
      //else if(actorPos.x == half || actorPos.y == half ||
      //    actorPos.x == -half || actorPos.y == -half) -0.01
      else 0.0

    if(reward > 0.01) log.debug("new reward! " + reward)

    currentAward = reward + speedReward + anomalyAward
//
    val treward =
      if(twithinReward) 1.0 - tdistFromReward / rewardDist.toDouble + 0.25
      //else if(tactorPos.x == half || tactorPos.y == half ||
      //    tactorPos.x == -half || tactorPos.y == -half) -0.01
      else 0.0

    //thalamus is called by full region at the right time

    //thalamus ! ThalamusActor.ReceiveSensoryInput(sensoryInput)

    thalamusOnly ! ThalamusActor.ReceiveInput(tsensoryInput)
    thalamusOnly ! ThalamusActor.Update(treward + tspeedReward)

    region.update(
      sensoryInput,
      thalamusOutput.get
    )
    thalamusOutput = None
    thalamusOnlyOutput = None

    updateStats()

    ticks += 1
  } else log.warning("missing external input!")


  def getRotationalVectorFromInput(input: CLA.Input): Vec2 = {
    val halfWidth = input.length / 2

    val vecs = for {
      idx <- 0 until input.length
      x = idx - halfWidth
      if input(idx)
    //} yield Vec2(x, 1).safeNormal
    } yield Vec2(x, math.abs(x) / halfWidth.toDouble).safeNormal

    val vecAvg = if(vecs.length > 0.0001) vecs.sum / vecs.length else Vec2.zero

    vecAvg.safeNormal

    /*//high index values are more weighted
    val (left0, right) = input.splitAt(halfWidth)
    val left = left0.reverse

    val maxPerSide = (0 until halfWidth).sum.toDouble

    val leftTotal = (0 until halfWidth).map {
      case idx if left(idx) => idx
      case _ => 0
    }.sum

    val rightTotal = (0 until halfWidth).map {
      case idx if right(idx) => idx
      case _ => 0
    }.sum

    val leftRatio = leftTotal / maxPerSide
    val rightRatio = rightTotal / maxPerSide
    val totalRatio = (leftRatio + rightRatio) / 2.0

    //x is rotational, y is forward. normalized
    Vec2(-leftRatio + rightRatio, totalRatio).safeNormal*/
  }

  override def preStart(): Unit = {
    super.preStart()

    log.info("goal game started")

    uiRef ! ServerCommands.GameIdentity
    context watch uiRef

    thalamus ! NeuralEntityActor.Subscribe
    thalamusOnly ! NeuralEntityActor.Subscribe
    context watch thalamus
    context watch thalamusOnly

    maybeRunSome()
  }

  override def postStop(): Unit = {
    super.postStop()
    posTimer.cancel()
  }

  def resume(): Unit = maybeRunSome()

  def clampWorldPos(initPos: Vec2): Vec2 = {
    var out = initPos

    /*if(out.x > halfGameDimensions) out = Vec2(halfGameDimensions, out.y)
    if(out.y > halfGameDimensions) out = Vec2(out.x, halfGameDimensions)
    if(out.x < -halfGameDimensions) out = Vec2(-halfGameDimensions, out.y)
    if(out.y < -halfGameDimensions) out = Vec2(out.x, -halfGameDimensions)*/

    while(out.x > halfGameDimensions) out -= Vec2(gameDimensions, 0)
    while(out.y > halfGameDimensions) out -= Vec2(0, gameDimensions)
    while(out.x < -halfGameDimensions) out += Vec2(gameDimensions, 0)
    while(out.y < -halfGameDimensions) out += Vec2(0, gameDimensions)

    out
  }

  def receive = neuralEntityActorReceive orElse baseGameReceive orElse {
    case ThalamusActor.ThalamusOutput(input) if sender == thalamus =>
      require(input.length == inputWidth, s"expected width $inputWidth, got ${input.length} ")
      thalamusOutput = Some(input)

      //motorSDR.update(motorSource)

      val partialInput = input//motorSDR.produce

      val extraSpeed = 1//if(withinReward) 5 else 1

      val rotVector = getRotationalVectorFromInput(partialInput)
      val dTheta = rotVector.x * turnSpeed
      val forwardRate = 0.2//rotVector.y

      actorMoveDir = rotVector

      /*if(!withinReward) */actorRotation += dTheta
      actorPos = actorPos + actorDirVec.safeNormal * forwardRate * actorSpeed * extraSpeed

      maybeRunSome()
    case ThalamusActor.ThalamusOutput(input) if sender == thalamusOnly =>
      require(input.length == inputWidth, s"expected width $inputWidth, got ${input.length} ")
      thalamusOnlyOutput = Some(input)

      val partialInput = input//.take(input.length)

      val extraSpeed = 1//if(twithinReward) 5 else 1

      val rotVector = getRotationalVectorFromInput(partialInput)
      val dTheta = rotVector.x * turnSpeed
      val forwardRate = 0.2//rotVector.y

      tactorMoveDir = rotVector

      /*if(!twithinReward) */tactorRotation += dTheta
      tactorPos = tactorPos + tactorDirVec.safeNormal * forwardRate * actorSpeed * extraSpeed

      maybeRunSome()
    case RunOne => maybeRunSome(force = true)
    case DoTick if tickRequested =>
      tickRequested = false
      runTick()
    case SendPos =>
      uiRef ! ActorPosition(actorPos, actorRotation)
      uiRef ! TActorPosition(tactorPos, tactorRotation)
    case Bump =>
      actorPos = Vec2.random * halfGameDimensions * math.random
      tactorPos = Vec2.random * halfGameDimensions * math.random
    case ToggleCorticalControl =>
      isCorticalMotorControlEnabled = !isCorticalMotorControlEnabled
      log.info("cortical control: " + isCorticalMotorControlEnabled)
  }
}
