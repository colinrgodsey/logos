package com.colingodsey.logos.cla.ui

import com.colingodsey.logos.cla.{L4Region, LearningColumn, L3Region, CLA}
import com.colingodsey.logos.cla.encoders.{WeightedScalarEncoder, ScalarEncoder}
import com.colingodsey.logos.collections.{Vec2, Vec3}
import json._

import org.scalajs.{dom => window}

import org.scalajs.jquery.{JQueryStatic, JQuery}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExport}

import js.Dynamic.global

import org.scalajs.dom.ext.{Color => DOMColor}

import scala.util.{Failure, Success, Random}

object MotorBrainMain extends MotorWorker()(JSExecutionContext.queue) with BaseWebWorkerMain {
  import MotorWorker._

  println("starting brain main worker")

  MotorWorker.registerPickling()

  def receive: PartialFunction[Any, Any] = {
    case GetStats => getUIStats
    case Move(p) => move(p)
  }

  def onBallMove(pos: Vec2): Unit = ParentRef ! BallMoved(pos)
}

abstract class MotorWebWorker(implicit ec: ExecutionContext) extends BaseWebWorker with MotorWorker.Interface {
  import MotorWorker._

  implicit val to = Mailbox.Timeout(10.seconds)

  println("starting worker")

  //get er loaded
  worker.postMessage("com.colingodsey.logos.cla.ui.MotorBrainMain().main();")

  def getUIStats: Future[RunStats] =
    (WorkerRef ? GetStats) map {
      case x: RunStats => x
    }

  def move(p: Vec2): Future[Vec2] = WorkerRef ? Move(p) map {
    case x: Vec2 => x
  }

  def receive: PartialFunction[Any, Any] = {
    case BallMoved(pos) => onBallMove(pos)
  }
}

object MotorWorker {
  implicit val config = CLA.ReducedConfig

  implicit val vec2acc = ObjectAccessor.of[Vec2]

  case object GetStats

  case class RunStats(l3Score: Double,
      l4Score: Double,
      activeDuties: IndexedSeq[Double],
      motorOverlap: IndexedSeq[Double],
      l3overlapDuties: IndexedSeq[Double],
      l4overlapDuties: IndexedSeq[Double]) {
    override lazy val hashCode: Int = productIterator.toSeq.hashCode
  }
  object RunStats {
    implicit val acc = ObjectAccessor.of[RunStats]
  }

  case class Move(pos: Vec2)
  object Move {
    implicit val acc = ObjectAccessor.of[Move]
  }

  case class BallMoved(pos: Vec2)
  object BallMoved {
    implicit val acc = ObjectAccessor.of[BallMoved]
  }

  trait Interface {
    def getUIStats: Future[RunStats]
    def move(p: Vec2): Future[Vec2]
    def onBallMove(pos: Vec2): Unit
  }

  def registerPickling(): Unit = {
    Mailbox.register()

    Mailbox.registry.addAccessor[Move]
    Mailbox.registry.addAccessor[RunStats]
    Mailbox.registry.addAccessor[BallMoved]
    Mailbox.registry.addAccessor[Vec2]
    Mailbox.registry.addSingleton(GetStats)
  }
}

abstract class MotorWorker(implicit ec: ExecutionContext) extends MotorWorker.Interface {
  import DOMExt._
  import MotorWorker._

  val worldRadius = 150.0
  val tickDelay = 0.millis
  val speed = 1.7
  val ballDist = 50
  val maxPauseDelayMs = 1300

  val encoder = new ScalarEncoder(config.inputWidth / 4, config.minOverlap + 5, min = -worldRadius, max = worldRadius)
  val targetEncoder = new WeightedScalarEncoder(config.inputWidth / 4, config.minOverlap + 5, min = -worldRadius, max = worldRadius)
  val mEncoder = new WeightedScalarEncoder(config.inputWidth / 2, config.minOverlap + 5, min = -1, max = 1)
  val region = new L4Region

  var pos = Vec2.zero
  var ballPos = Vec2(10, 10)
  var l3ScoreBuffer = List[Double]()
  var l4ScoreBuffer = List[Double]()

  var movePauseDeadline = Deadline.now
  var movePaused = false

  def move(move0: Vec2): Future[Vec2] = Future {
    val move = if(movePaused) Vec2.zero else move0.safeNormal

    //apply move
    val newPos = pos + move * speed
    val ballVec = newPos - ballPos

    val newBallPos = if(ballVec.length < ballDist) {
      ballPos - ballVec.normal
    } else {
      ballPos + ballVec.normal * 0.1
    }

    if(newBallPos.y >= -worldRadius && newBallPos.y < worldRadius &&
        newBallPos.x >= -worldRadius && newBallPos.x < worldRadius) {
      ballPos = newBallPos
      onBallMove(ballPos)
    }

    if(newPos.y >= -worldRadius && newPos.y < worldRadius &&
        newPos.x >= -worldRadius && newPos.x < worldRadius)
      pos = newPos

    if(movePauseDeadline.isOverdue) {
      movePauseDeadline = Deadline.now + (maxPauseDelayMs * math.random).millis
      movePaused = !movePaused
    }

    val newBallVec = pos - ballPos

    val xEnc = encoder.encode(pos.x)
    val yEnc = encoder.encode(pos.y)

    val bxEnc = targetEncoder.encode(newBallVec.x, 1.0 / (math.abs(newBallVec.x) / worldRadius * 2))
    val byEnc = targetEncoder.encode(newBallVec.y, 1.0 / (math.abs(newBallVec.y) / worldRadius * 2))

    val xmEnc = mEncoder.encode(move.x, math abs move.x)
    val ymEnc = mEncoder.encode(move.y, math abs move.y)

    val input = xEnc.iterator.toStream ++ yEnc.iterator ++ bxEnc.iterator.toStream ++ byEnc.iterator
    val motor = xmEnc.iterator.toStream ++ ymEnc.iterator

    region.update(input, motor)

    l3ScoreBuffer = region.l3Layer.anomalyScore +: l3ScoreBuffer
    l4ScoreBuffer = region.l4Layer.anomalyScore +: l4ScoreBuffer

    pos
  }

  def getUIStats: Future[RunStats] = Future successful {
    val l3score = if(l3ScoreBuffer.isEmpty) 0 else l3ScoreBuffer.sum / l3ScoreBuffer.length
    l3ScoreBuffer = Nil

    val l4score = if(l4ScoreBuffer.isEmpty) 0 else l4ScoreBuffer.sum / l4ScoreBuffer.length
    l4ScoreBuffer = Nil

    RunStats(
      l3Score = l3score,
      l4Score = l4score,
      activeDuties = region.inputLayer.segments.map(_.activeDutyCycle.toDouble),
      //overlapDuties = region.inputLayer.segments.map(_.overlapDutyCycle.toDouble)
      motorOverlap = region.motorInput.segments.map(_.activeOverlap),
      l4overlapDuties = region.inputLayer.segments.map(_.activeOverlap),
      l3overlapDuties = region.l4Input.segments.map(_.activeOverlap)
    )
  }
}

object MotorTest extends js.JSApp { app =>
  import DOMExt._
  import MotorWorker.config

  implicit val ec = CLA.VM.newDefaultExecutionContext

  val uiDelay = 230.millis
  val moveDelay = (1 / 20.0).seconds
  val moveDuration = 5.minutes

  ChartJS.Chart.GlobalDefaults.animationSteps = 15//35
  ChartJS.Chart.LineDefaults.datasetFill = false
  ChartJS.Chart.PolarDefaults.animationSteps = ChartJS.Chart.GlobalDefaults.animationSteps
  ChartJS.Chart.PolarDefaults.animationEasing = "easeOutQuart"

  val activeDutyChart = new ColumnPolarComponent($("#activeDutyChart"), config.regionWidth)
  val l3overlapChart = new ColumnPolarComponent($("#l3overlapDutyChart"), config.regionWidth)
  val l4overlapChart = new ColumnPolarComponent($("#l4overlapDutyChart"), config.regionWidth)
  val motorOverlap = new ColumnPolarComponent($("#motorOverlap"), config.regionWidth)
  val anomaly = new ColumnLineComponent($("#anomalyChart"), Seq("l3anomaly", "l4anomaly"))
  val ticks = $("#ticks")
  val actor = $("#world #actor")
  val ball = $("#world #ball")
  val started = Deadline.now

  val workerInstance = if(window.Worker != null) {
    new MotorWebWorker {
      def onBallMove(pos: Vec2): Unit = ballPosChange(pos)
    }
  } else new MotorWorker {
    def onBallMove(pos: Vec2): Unit = ballPosChange(pos)
  }

  var isRandomMoving = false
  var lastStats: Option[MotorWorker.RunStats] = None

  def posChange(p: Vec2): Unit = {
    actor.css("left", p.x + "px")
    actor.css("top", p.y + "px")
  }

  def ballPosChange(p: Vec2): Unit = {
    ball.css("left", p.x + "px")
    ball.css("top", p.y + "px")
  }

  def update(): Unit = workerInstance.getUIStats onComplete {
    case Success(stats) =>
      //println(stats)
      val f = if(Some(stats.hashCode) != lastStats.map(_.hashCode)) ChartJS.Chart.onAnimationComplete {
        activeDutyChart.update(stats.activeDuties)
        motorOverlap.update(stats.motorOverlap)
        l3overlapChart.update(stats.l3overlapDuties)
        l4overlapChart.update(stats.l4overlapDuties)
        anomaly.update(stats.l3Score, stats.l4Score)
        lastStats = Some(stats)
      } else Future.successful()

      for {
        _ <- f
        _ <- timerFuture(uiDelay)
      } update()
    case Failure(t) => throw t
  }

  def move(v: Vec2) = {
    val f = workerInstance.move(v)
    f foreach posChange
    f
  }

  def moveRandomly(started: Deadline = Deadline.now): Unit =
    if(!isRandomMoving && (started + moveDuration).hasTimeLeft) {
      isRandomMoving = true

      val t0 = (-started.timeLeft).toMillis
      val t = t0 * 0.0001

      val x0 =
        math.cos(t * 1) +
            math.cos(t * 5) +
            math.cos(t * 30) +
            math.cos(t * 6.2)

      val x = x0 / 4.0

      val y0 =
        math.sin(t * 1) +
            math.cos(t * 5.2) +
            math.cos(t * 25) +
            math.cos(t * 7)

      val y = y0 / 4.0

      val v = Vec2(x, y)

      move(v).flatMap(_ => timerFuture(moveDelay)) onComplete { _ =>
        isRandomMoving = false
        moveRandomly(started)
      }
    }

  $(global.document).keypress { e: window.KeyboardEvent =>
    e.keyCode match {
      case 13 if !e.shiftKey =>
        restartRun()
        false
      case 'w' => move(Vec2(0, 1))
      case 's' => move(Vec2(0, -1))
      case 'a' => move(Vec2(-1, 0))
      case 'd' => move(Vec2(1, 0))
      case 'q' => moveRandomly()
      case _ => true
    }
  }

  def restartRun(): Unit = {
    posChange(Vec2.zero)
  }

  @JSExport
  def main(): Unit = {
    import Flot._
    import DOMExt._

    println("loading...")

    MotorWorker.registerPickling()

    restartRun()
    update()
    moveRandomly()
  }
}