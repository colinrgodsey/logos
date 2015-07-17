package com.colingodsey.logos.cla.ui

import com.colingodsey.logos.cla.{L4Region, LearningColumn, L3Region, CLA}
import com.colingodsey.logos.cla.encoders.ScalarEncoder
import com.colingodsey.logos.collections.Vec3
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

object BrainMain extends Worker()(JSExecutionContext.queue) with BaseWebWorkerMain {
  import Worker._

  println("starting brain main worker")

  Worker.registerPickling()

  def receive: PartialFunction[Any, Any] = {
    case StartRun(f) => startRun(f)
    case GetStats => getUIStats
  }
}

class WebWorker(implicit ec: ExecutionContext) extends BaseWebWorker with Worker.Interface {
  import Worker._

  implicit val to = Mailbox.Timeout(10.seconds)

  println("starting worker")

  //get er loaded
  worker.postMessage("com.colingodsey.logos.cla.ui.BrainMain().main();")

  def startRun(func: String): Unit =
    WorkerRef ! StartRun(func)

  def getUIStats: Future[RunStats] =
    (WorkerRef ? GetStats) map {
      case x: RunStats => x
    }

  def receive: PartialFunction[Any, Any] = PartialFunction.empty
}

object Worker {
  implicit val config = CLA.ReducedConfig

  case object GetStats
  case class StartRun(func: String)
  object StartRun {
    implicit val acc = ObjectAccessor.of[StartRun]
  }
  case class RunStats(anomalyScore: Double, activeDuties: IndexedSeq[Double],
      overlapDuties: IndexedSeq[Double], ticks: Int)
  object RunStats {
    implicit val acc = ObjectAccessor.of[RunStats]
  }

  trait Run {
    def cancel(): Unit
  }

  trait Interface {
    def startRun(func: String): Unit
    def getUIStats: Future[RunStats]
  }

  def registerPickling(): Unit = {
    Mailbox.register()

    Mailbox.registry.addAccessor[RunStats]
    Mailbox.registry.addAccessor[StartRun]
    Mailbox.registry.addSingleton(GetStats)
  }
}

class Worker(implicit ec: ExecutionContext) extends Worker.Interface {
  import DOMExt._
  import Worker._

  var currentRun: Option[Run] = None
  var scoreBuffer = List[Double]()

  val encoder = new ScalarEncoder(config.inputWidth, config.minOverlap + 5)
  val region = new L4Region
  //val region = new L3Region
  val tickDelay = 0.millis
  var ticks = 0

  def startRun(jsFunctionBody: String): Unit = {
    currentRun.foreach(_.cancel())
    currentRun = None

    println("starting run....")

    val running = new Run {
      ticks = 0
      var running = false
      val deadline = Deadline.now + 40.seconds

      val jsFunction = js.Dynamic.newInstance(global.Function)(js.Array("i"), jsFunctionBody).asInstanceOf[js.Function1[Double, Double]]

      def tick(): Unit = if(running) try {
        val r = jsFunction(ticks)

        val started = Deadline.now

        while(Deadline.now - started < 10.millis) {

          region.update(encoder.encode(r / 2.0))

          scoreBuffer = region.l3Layer.anomalyScore +: scoreBuffer

          ticks += 1
        }

        if(ticks > 100000) cancel()
        if(deadline.isOverdue) cancel()

        timerFuture(tickDelay).flatMap(_ => Future(tick()))
      } catch {
        case t: Throwable =>
          log("Caught error " + t.getMessage)
          error(t)
      }

      def start(): Unit = if(!running) {
        running = true

        Future(tick())
      }

      def cancel(): Unit = if(running) {
        running = false
      }
    }

    running.start()
    currentRun = Some(running)
  }

  def getUIStats: Future[RunStats] = Future successful {
    val score = if(scoreBuffer.isEmpty) 0 else scoreBuffer.sum / scoreBuffer.length
    scoreBuffer = Nil

    RunStats(
      anomalyScore = score,
      activeDuties = region.inputLayer.segments.map(_.activeDutyCycle.toDouble),
      //overlapDuties = region.inputLayer.segments.map(_.overlapDutyCycle.toDouble),
      overlapDuties = region.inputLayer.segments.map(_.overlap),
      ticks = ticks
    )
  }
}

object UI extends js.JSApp {
  import DOMExt._
  import Worker.config

  implicit val ec = CLA.VM.newDefaultExecutionContext

  ChartJS.Chart.GlobalDefaults.animationSteps = 35
  ChartJS.Chart.PolarDefaults.animationSteps = ChartJS.Chart.GlobalDefaults.animationSteps
  ChartJS.Chart.PolarDefaults.animationEasing = "easeOutQuart"

  val uiDelay = 10.millis

  val activeDutyChart = new ColumnPolarComponent($("#activeDutyChart"), config.regionWidth)
  val overlapChart = new ColumnPolarComponent($("#overlapDutyChart"), config.regionWidth)
  val anomaly = new ColumnLineComponent($("#anomalyChart"), Seq("anomaly"))
  val ticks = $("#ticks")

  val workerInstance = if(window.Worker != null) {
    new WebWorker
  } else new Worker

  def jsFunctionBody = $("#inputFunction")(0).asInstanceOf[js.Dynamic].value.asInstanceOf[String]

  def update(): Unit = workerInstance.getUIStats onComplete {
    case Success(stats) =>
      //println(stats)
      val f = ChartJS.Chart.onAnimationComplete {
        activeDutyChart.update(stats.activeDuties)
        overlapChart.update(stats.overlapDuties)
        anomaly.update(stats.anomalyScore)

        ticks.text("Ticks: " + stats.ticks)
      }

      for {
        _ <- f
        _ <- timerFuture(uiDelay)
      } update()
    case Failure(t) => throw t
  }

  $(global.document).keypress { e: window.KeyboardEvent =>
    e.keyCode match {
      case 13 if !e.shiftKey =>
        restartRun()
        false
      case _ => true
    }
  }

  def restartRun(): Unit = {
    workerInstance.startRun(jsFunctionBody)
  }

  @JSExport
  def main(): Unit = {
    import Flot._
    import DOMExt._

    println("loading...")

    Worker.registerPickling()

    restartRun()
    update()
    /*
        val data = Flot.Data(Seq(0.0 -> 0.0, 1.0 -> 1.0))

        val xaxis = Axis(color = Color.Blue)

        val options = Options(xaxis = xaxis, yaxis = xaxis)

        $("#testflot").plot(data, options)

        val chart = ChartJS.Chart($("#myChart"))

        val sets = js.Array(
          ChartJS.LineDataSet("a", js.Array(1, 2)),
          ChartJS.LineDataSet("b", js.Array(1, 2))
        )
        val lineData = ChartJS.LineData(js.Array("a", "b"), sets)
        val line = chart.Line(lineData)*/

  }
}