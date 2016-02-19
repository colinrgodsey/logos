package com.colingodsey.logos.cla.ui

import akka.actor._
import akka.actor.pattern._
import com.colingodsey.logos.akka.WebSocketBridge
import com.colingodsey.logos.cla.server.ServerCommands
import com.colingodsey.logos.cla.server.ServerCommands.GoalGame.ToggleCorticalControl
import com.colingodsey.logos.cla.server.ServerCommands.{GetStats, RunStats, Layer, ColumnView}
import com.colingodsey.logos.collections.Vec2
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLCanvasElement

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportDescendentObjects, JSExport}

import js.Dynamic.global

import org.scalajs.dom.ext.{Color => DOMColor}
import org.scalajs.{dom => window}

import org.scalajs.jquery.{JQueryStatic, JQuery}

import scala.concurrent.duration._

import DOMExt._

class GoalGame(keyPressActor: ActorRef) extends JSFrameActor with Actor with ActorLogging {
  import KeyPressActor.KeyPress
  import GoalGame._
  import ServerCommands._
  import ServerCommands.GoalGame._
  import context.dispatcher
  import context.system.scheduler

  ChartJS.Chart.GlobalDefaults.animationSteps = 30//35
  ChartJS.Chart.LineDefaults.datasetFill = false
  ChartJS.Chart.PolarDefaults.animationSteps = ChartJS.Chart.GlobalDefaults.animationSteps
  ChartJS.Chart.PolarDefaults.animationEasing = "easeOutQuart"

  val uiDelay = 18.millis

  val regionWidth = 128
  val chartSize = 400
  val gameDimensions = 400

  val gameboard = $("#gameboard")
  val canvas = gameboard.get(0).asInstanceOf[HTMLCanvasElement]
  val canvasContext = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

  val activeDutyChart = new ColumnPolarComponent($("#activeDutyChart"), regionWidth, chartSize)
  val l3overlapChart = new ColumnPolarComponent($("#l3overlapDutyChart"), regionWidth, chartSize)
  val l4overlapChart = new ColumnPolarComponent($("#l4overlapDutyChart"), regionWidth, chartSize)
  val l5overlapChart = new ColumnPolarComponent($("#l5overlapDutyChart"), regionWidth, chartSize)
  val l6overlapChart = new ColumnPolarComponent($("#l6overlapDutyChart"), regionWidth, chartSize)
  val anomaly = new ColumnLineComponent($("#anomalyChart"), Seq("l3", "l4", "l5", "l6"))
  val ticks = $("#ticks")
  val columnView = new ColumnView($("#columnView"))

  val bridgeProps = Props(classOf[WebSocketBridge],
    s"ws://localhost:7131/")
  val bridge = context.actorOf(bridgeProps, name = "ws-bridge")

  var isPaused = false
  var gameRef = context.system.deadLetters
  var gettingStats = false
  var animationFrameRequested = false

  var actorPos: Vec2 = Vec2.zero
  var actorRotation: Double = 0.0
  var tactorPos: Vec2 = Vec2.zero
  var tactorRotation: Double = 0.0

  def actorDirVec: Vec2 = Vec2(math.cos(actorRotation), math.sin(actorRotation))
  def tactorDirVec: Vec2 = Vec2(math.cos(tactorRotation), math.sin(tactorRotation))

  def toDisplayCoords(x: Vec2): Vec2 =
    x + Vec2.one * gameDimensions * 0.5

  def actorDisplayPos = toDisplayCoords(actorPos)
  def tactorDisplayPos = toDisplayCoords(tactorPos)

  def maybeGetStats(): Unit = if(!gettingStats && !isPaused) {
    gettingStats = true

    gameRef ! GetStats
  }

  override def preStart(): Unit = {
    super.preStart()

    log.info("starting goal game.... " + bridge)
    context watch bridge

    bridge ! WebSocketBridge.Identify(ServerCommands.GoalGame.Start(regionWidth))

    keyPressActor ! KeyPressActor.Subscribe

    gameboard.removeClass("hidden")

    drawBoard()
  }

  override def postStop(): Unit = {
    super.postStop()

    gameboard.addClass("hidden")

    activeDutyChart.destroy()
    l3overlapChart.destroy()
    l4overlapChart.destroy()
    l5overlapChart.destroy()
    l6overlapChart.destroy()
    anomaly.destroy()
  }

  def drawBoard(): Unit = {
    val ctx = canvasContext

    val cellRadius = 6

    val markerLineDest = actorDisplayPos + actorDirVec * (cellRadius * 1.2)
    val tmarkerLineDest = tactorDisplayPos + tactorDirVec * (cellRadius * 1.2)
    val zeroDisplay = toDisplayCoords(Vec2.zero)

    ctx.clearRect(0, 0, canvas.width, canvas.height)
    ctx.save()

    ctx.beginPath()
    ctx.arc(zeroDisplay.x, zeroDisplay.y, 80 /*this changes... */, 0, 2 * math.Pi, false)
    ctx.fillStyle = "yellow"
    ctx.fill()
    ctx.lineWidth = 2
    ctx.strokeStyle = "yellow"
    ctx.stroke()

    ctx.beginPath()
    ctx.arc(actorDisplayPos.x, actorDisplayPos.y, cellRadius, 0, 2 * math.Pi, false)
    ctx.fillStyle = "black"
    ctx.fill()
    ctx.lineWidth = 2
    ctx.strokeStyle = "black"
    ctx.stroke()

    ctx.beginPath()
    ctx.moveTo(actorDisplayPos.x, actorDisplayPos.y)
    ctx.lineTo(markerLineDest.x, markerLineDest.y)
    ctx.closePath()
    ctx.lineWidth = 2
    ctx.strokeStyle = "red"
    ctx.stroke()

    ctx.beginPath()
    ctx.arc(tactorDisplayPos.x, tactorDisplayPos.y, cellRadius, 0, 2 * math.Pi, false)
    ctx.fillStyle = "blue"
    ctx.fill()
    ctx.lineWidth = 2
    ctx.strokeStyle = "blue"
    ctx.stroke()

    ctx.beginPath()
    ctx.moveTo(tactorDisplayPos.x, tactorDisplayPos.y)
    ctx.lineTo(tmarkerLineDest.x, tmarkerLineDest.y)
    ctx.closePath()
    ctx.lineWidth = 2
    ctx.strokeStyle = "red"
    ctx.stroke()

    ctx.restore()
  }

  def receive = {
    case ServerCommands.GameIdentity =>
      gameRef = sender

      log.info("Game identified at " + gameRef)

      context watch gameRef

      gameRef ! StartAuto
      maybeGetStats()

    case KeyPress('c', _) =>
      gameRef ! ToggleCorticalControl

    case KeyPress('b', _) =>
      gameRef ! Bump
    case KeyPress('p', _) if isPaused =>
      isPaused = false
      gameRef ! StartAuto
      maybeGetStats()
    case KeyPress('p', _) =>
      isPaused = true
      gameRef ! StopAuto
    case KeyPress('t', _) => //if isPaused =>
      gameRef ! RunOne
      maybeGetStats()
      gameRef ! GetColumnView(Layer.L3)

    case stats: RunStats =>
      val animFut = ChartJS.Chart.onAnimationComplete {
        activeDutyChart.update(stats.activeDuties)
        l3overlapChart.update(stats.l3overlapDuties)
        l4overlapChart.update(stats.l4overlapDuties)
        l5overlapChart.update(stats.l5overlapDuties)
        l6overlapChart.update(stats.l6overlapDuties)
        anomaly.update(
          stats.l3anomalyScore,
          stats.l4anomalyScore,
          stats.l5anomalyScore,
          stats.l6anomalyScore
        )

        ticks.text("Ticks: " + stats.ticks)
      }

      val fut = for {
        _ <- animFut
        _ <- timerFuture(10.millis)
      } yield UIUpdated

      fut pipeTo self

    case UIUpdated =>
      gettingStats = false
      maybeGetStats()

    case ActorPosition(newPos, newRot) =>
      actorPos = newPos
      actorRotation = newRot

    case TActorPosition(newPos, newRot) =>
      tactorPos = newPos
      tactorRotation = newRot

    case JSFrameActor.AnimationFrame =>
      drawBoard()

    case x: ColumnView.Data =>
      columnView.update(x)
  }
}

object GoalGame {
  private case object UIUpdated
}

@JSExport class GoalGameMain extends CLAGame {

  println("loading goal game....")

  ServerCommands.register()

  val system = ActorSystem("UI")

  val keyPressActor = system.actorOf(Props[KeyPressActor], name = "key-press")

  val props = Props(classOf[Guardian], Props(classOf[GoalGame], keyPressActor))
  system.actorOf(props, name = "goal-game")

  @JSExport
  def stop(): Unit = {
    system.shutdown()
  }
}