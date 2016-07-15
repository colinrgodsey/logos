package com.colingodsey.logos.cla.ui
/*
import com.colingodsey.logos.cla.ui.ColumnView.Data
import com.colingodsey.logos.cla.{FullRegion, L4Region, L3Region, CLA}
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

import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success, Random}

object BrainMain extends Worker()(JSExecutionContext.queue) with BaseWebWorkerMain {
  import Worker._

  println("starting brain main worker")

  Worker.registerPickling()

  def receive: PartialFunction[Any, Any] = {
    case StartRun(f) => startRun(f)
    case GetStats => getUIStats
    case Start => start()
    case Stop => stop()
    case Tick => tick()
    case GetColumnView => getColumnView()
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

  def start() = WorkerRef ! Start
  def stop() = WorkerRef ! Stop
  def tick() = WorkerRef ! Tick

  def getColumnView(): Future[Data] =
    (WorkerRef ? GetColumnView) map {
      case x: Data => x
    }
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

  case object Stop
  case object Start
  case object Tick
  case object GetColumnView

  trait Run {
    def tick(): Unit
    def cancel(): Unit
  }

  trait Interface {
    def startRun(func: String): Unit
    def getUIStats: Future[RunStats]
    def stop(): Unit
    def start(): Unit
    def tick(): Unit
    def getColumnView(): Future[ColumnView.Data]
  }

  def registerPickling(): Unit = {
    import ColumnView.columnAcc

    Mailbox.register()

    Mailbox.registry.addAccessor[RunStats]
    Mailbox.registry.addAccessor[StartRun]
    Mailbox.registry.addAccessor[ColumnView.Data]
    Mailbox.registry.addSingleton(GetStats)
    Mailbox.registry.addSingleton(Stop)
    Mailbox.registry.addSingleton(Start)
    Mailbox.registry.addSingleton(Tick)
    Mailbox.registry.addSingleton(GetColumnView)
  }
}

class Worker(implicit ec: ExecutionContext) extends Worker.Interface {
  import DOMExt._
  import Worker._

  var currentRun: Option[Run] = None
  var scoreBuffer = List[Double]()

  val encoder = new ScalarEncoder(config.inputWidth, config.minOverlap + 5)
  //val region = new L4Region
  //val region = new L3Region
  val region = new FullRegion
  val tickDelay = 0.millis
  var ticks = 0
  var lastFunction = ""

  def startRun(jsFunctionBody: String): Unit = {
    currentRun.foreach(_.cancel())
    currentRun = None
    lastFunction = jsFunctionBody

    println("starting run....")

    val running = new Run {
      ticks = 0
      var running = false
      val deadline = Deadline.now + 5.minutes

      val jsFunction = js.Dynamic.newInstance(global.Function)(
        js.Array("i"), jsFunctionBody).asInstanceOf[js.Function1[Double, Double]]

      def tick(): Unit = tick(force = true)

      def tick(force: Boolean): Unit = if(running || force) try {
        val r = jsFunction(ticks)

        val started = Deadline.now

        def inner(): Unit = {
          //if(math.random < 0.05) println(encoder.encode(r / 2.0))
          region.update(encoder.encode(r / 2.0))

          scoreBuffer = region.l3Layer.anomalyScore +: scoreBuffer

          ticks += 1
        }

        if(force) inner()
        else while(Deadline.now - started < 10.millis) inner()

        if(ticks > 100000) cancel()
        if(deadline.isOverdue) cancel()

        if(running)
          timerFuture(tickDelay).flatMap(_ => Future(tick(force = false)))
      } catch {
        case t: Throwable =>
          log("Caught error " + t.getMessage)
          t.printStackTrace()
          error(t)
      }

      def start(): Unit = if(!running) {
        running = true

        Future(tick(force = false))
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
      activeDuties = region.l3Layer.columns.map(_.activeDutyCycle.toDouble),
      //overlapDuties = region.inputLayer.segments.map(_.overlapDutyCycle.toDouble),
      overlapDuties = region.l3Layer.columns.map(_.overlap),
      ticks = ticks
    )
  }

  def getColumnView: Future[ColumnView.Data] = Future fromTry Try {
    val columns = region.l3Layer.columns map { column =>
      val cells = column.cells map { cell =>
        ColumnView.Cell(cell.active, cell.activeForTicks > 1)
      }
      ColumnView.Column(cells, active = column.active,
        wasPredicted = column.wasPredicted)
    }

    ColumnView.Data(columns)
  }

  def stop() = currentRun.foreach(_.cancel())
  def tick() = currentRun.foreach(_.tick())
  def start() = startRun(lastFunction)
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
  val columnView = new ColumnView($("#columnView"))

  val workerInstance = if(window.Worker != null) {
    new WebWorker
  } else new Worker

  var isPaused = false

  var updateTimer: Option[JSScheduler.Cancellable] = None

  def jsFunctionBody = $("#inputFunction")(0).asInstanceOf[js.Dynamic].value.asInstanceOf[String]

  def update(): Unit = {
    updateTimer.foreach(_.cancel())
    updateTimer = Some(JSScheduler.scheduleEvenly(uiDelay)(doUpdate()))
  }

  def doUpdate(): Future[Unit] = workerInstance.getUIStats flatMap { stats =>
    ChartJS.Chart.onAnimationComplete {
      activeDutyChart.update(stats.activeDuties)
      overlapChart.update(stats.overlapDuties)
      anomaly.update(stats.anomalyScore)

      ticks.text("Ticks: " + stats.ticks)
    }.recover {
      case NonFatal(t) => t.printStackTrace()
    }
  }

  def updateColumnView(): Future[Unit] = {
    workerInstance.getColumnView map columnView.update recover {
      case NonFatal(t) => t.printStackTrace()
    }
  }

  $(global.document).keypress { e: window.KeyboardEvent =>
    e.keyCode match {
      case 13 if !e.shiftKey =>
        restartRun()
        update()
        false
      case 'p' if isPaused =>
        isPaused = false
        workerInstance.start()
        update()
      case 'p' =>
        isPaused = true
        workerInstance.stop()
        updateTimer.foreach(_.cancel())
      case 't' if isPaused =>
        workerInstance.tick()
        doUpdate()
        updateColumnView()
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
}*/