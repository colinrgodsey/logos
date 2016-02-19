package com.colingodsey.logos.cla.ui

import akka.actor._
import akka.actor.pattern._
import com.colingodsey.logos.akka.WebSocketBridge
import com.colingodsey.logos.cla.server.ServerCommands
import com.colingodsey.logos.cla.server.ServerCommands.{GetStats, RunStats, Layer, ColumnView}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportDescendentObjects, JSExport}

import js.Dynamic.global

import org.scalajs.dom.ext.{Color => DOMColor}
import org.scalajs.{dom => window}

import org.scalajs.jquery.{JQueryStatic, JQuery}

import scala.concurrent.duration._

import DOMExt._

class SineGame(keyPressActor: ActorRef) extends Actor with ActorLogging {
  import KeyPressActor.KeyPress
  import SineGame._
  import ServerCommands._
  import ServerCommands.SineGame._
  import context.dispatcher
  import context.system.scheduler

  ChartJS.Chart.GlobalDefaults.animationSteps = 30//35
  ChartJS.Chart.LineDefaults.datasetFill = false
  ChartJS.Chart.PolarDefaults.animationSteps = ChartJS.Chart.GlobalDefaults.animationSteps
  ChartJS.Chart.PolarDefaults.animationEasing = "easeOutQuart"

  val uiDelay = 10.millis

  val regionWidth = 128
  val chartSize = 400

  val activeDutyChart = new ColumnPolarComponent($("#activeDutyChart"), regionWidth, chartSize)
  val l3overlapChart = new ColumnPolarComponent($("#l3overlapDutyChart"), regionWidth, chartSize)
  val l4overlapChart = new ColumnPolarComponent($("#l4overlapDutyChart"), regionWidth, chartSize)
  val l5overlapChart = new ColumnPolarComponent($("#l5overlapDutyChart"), regionWidth, chartSize)
  val l6overlapChart = new ColumnPolarComponent($("#l6overlapDutyChart"), regionWidth, chartSize)
  val anomaly = new ColumnLineComponent($("#anomalyChart"), Seq("l3", "l4", "l5", "l6"))
  val ticks = $("#ticks")
  val columnView = new ColumnView($("#columnView"))

  val bridgeProps = Props(classOf[WebSocketBridge], "ws://localhost:7131/")
  val bridge = context.actorOf(bridgeProps, name = "ws-bridge")

  var tickCounter = 0
  var isPaused = false
  var gameRef = context.system.deadLetters
  var currentJSFunction = createJSFunction()
  var gettingStats = false

  def jsFunctionBody = $("#inputFunction")(0).asInstanceOf[js.Dynamic].value.asInstanceOf[String]

  def createJSFunction(): js.Function1[Double, Double] = {
    js.Dynamic.newInstance(global.Function)(
      js.Array("i"), jsFunctionBody).asInstanceOf[js.Function1[Double, Double]]
  }

  def nextPoint(): Double = {
    tickCounter += 1

    currentJSFunction(tickCounter.toDouble)
  }

  def maybeGetStats(): Unit = if(!gettingStats && !isPaused) {
    gettingStats = true

    gameRef ! GetStats
  }

  override def preStart(): Unit = {
    super.preStart()

    log.info("starting sine game.... " + bridge)

    context watch bridge

    bridge ! WebSocketBridge.Identify(ServerCommands.SineGame.Start(regionWidth))

    keyPressActor ! KeyPressActor.Subscribe
  }

  override def postStop(): Unit = {
    super.postStop()

    activeDutyChart.destroy()
    l3overlapChart.destroy()
    l4overlapChart.destroy()
    l5overlapChart.destroy()
    l6overlapChart.destroy()
    anomaly.destroy()
  }

  def receive = {
    case ServerCommands.GameIdentity =>
      gameRef = sender

      log.info("Game identified at " + gameRef)

      context watch gameRef

      gameRef ! StartAuto
      maybeGetStats()

    case NeedDataPoints(n) =>
      sender ! DataPoints(Array.fill(n)(nextPoint()))

    case KeyPress(13, false) =>
      currentJSFunction = createJSFunction()
      gameRef ! ClearPoints
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

    case x: ColumnView.Data =>
      columnView.update(x)
  }
}

object SineGame {
  private case object UIUpdated
}

@JSExport class SineGameMain extends CLAGame {
  println("loading sine game....")

  ServerCommands.register()

  val system = ActorSystem("UI")

  val keyPressActor = system.actorOf(Props[KeyPressActor], name = "key-press")

  val props = Props(classOf[Guardian], Props(classOf[SineGame], keyPressActor))
  system.actorOf(props, name = "sine-game")

  @JSExport
  def stop(): Unit = {
    system.shutdown()
  }
}