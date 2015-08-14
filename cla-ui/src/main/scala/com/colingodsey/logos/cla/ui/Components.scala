package com.colingodsey.logos.cla.ui

import com.colingodsey.logos.collections.Vec3
import org.scalajs.{dom => window}
import org.scalajs.dom.ext.{Color => DOMColor}
import org.scalajs.jquery.{JQuery, JQueryStatic}

import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.util.{Failure, Success, Random}


object DOMExt {
  implicit def Color2Color(dc: DOMColor): Color = dc.toString.asInstanceOf[Color]

  sealed trait Color extends js.Any
  val Color = DOMColor

  implicit def FlotStaticExt(x: JQueryStatic): FlotStatic = x.asInstanceOf[FlotStatic]
  implicit def FlotExt(x: JQuery): Flot = x.asInstanceOf[Flot]

  implicit class IterableExt[T](x: TraversableOnce[T]) {
    def toJsArray: js.Array[T] = js.Array(x.toSeq: _*)
  }

  val $ = js.Dynamic.global.$.asInstanceOf[JQueryStatic]

  def log(x: Any) = js.Dynamic.global.console.log(x.asInstanceOf[js.Any])
  def error(x: Throwable): Unit = js.Dynamic.global.console.error(x.asInstanceOf[js.Any])
  def error(x: String): Unit = js.Dynamic.global.console.error(x.asInstanceOf[js.Any])

  def randomColor = {
    val intensity = math.random * 0.8 + 0.2
    val v = Vec3.random * intensity * 256

    Color(math.abs(v.x.toInt), math.abs(v.y.toInt), math.abs(v.z.toInt))
  }

  def randomColor(s: Int) = {
    val r = new Random(s + 1)
    val intensity = r.nextDouble * 0.8 + 0.2
    val v = Vec3.random(s) * intensity * 256

    Color(math.abs(v.x.toInt), math.abs(v.y.toInt), math.abs(v.z.toInt))
  }

  def timerFuture(d: FiniteDuration): Future[Unit] = {
    val p = Promise[Unit]()

    js.timers.setTimeout(d) {
      p.success {}
    }

    p.future
  }
}

object Scheduler {
  trait Cancelable {
    def cancel(): Unit

    private[Scheduler] var isCanceled = false
  }

  def scheduleOnce(dur: FiniteDuration)(f: => Unit): Cancelable = new Cancelable {
    private val id = js.timers.setTimeout(dur) {
      if(!isCanceled) f
      isCanceled = true
    }

    def cancel(): Unit = if(!isCanceled) {
      js.timers.clearTimeout(id)
      isCanceled = true
    }
  }

  def scheduleEvery(every: FiniteDuration)(f: => Unit): Cancelable = new Cancelable {
    private val id = js.timers.setInterval(every) {
      if(!isCanceled) f
      isCanceled = true
    }

    def cancel(): Unit = if(!isCanceled) {
      js.timers.clearInterval(id)
      isCanceled = true
    }
  }

  def scheduleEvenly(dur: FiniteDuration)(f: => Future[_])(implicit ec: ExecutionContext): Cancelable = new Cancelable {
    private var id = js.timers.setTimeout(dur)(inner)

    def inner(): Unit = if(!isCanceled) {
      val fut = f

      fut.onComplete {
        case Success(_) =>
          id = js.timers.setTimeout(dur)(inner)
        case Failure(t) =>
          t.printStackTrace()
      }
    }

    def cancel(): Unit = if(!isCanceled) {
      js.timers.clearTimeout(id)
      isCanceled = true
    }
  }
}

class ColumnPolarComponent(sel: JQuery, width: Int) {
  import DOMExt._
  import ChartJS._

  private implicit def ec = JSExecutionContext.queue

  val sets = (0 until width).map { i =>
    val s = i
    PolarDataSet(0.0, color = randomColor(s), highlight = randomColor(s + 1), label = i.toString)
  }.toIndexedSeq

  val chart = Chart(sel).PolarArea(sets.toJsArray, Chart.PolarDefaults)

  def update(values: IndexedSeq[Double]): Unit = Future {
    require(values.length == width)

    (0 until width).map { i =>
      val set = chart.segments(i)

      set.value = values(i)
    }

    chart.update()
  }
}

class ColumnLineComponent(sel: JQuery, labels: Seq[String], max: Int = 120) {
  import DOMExt._
  import ChartJS._

  labels map { label =>

  }

  val segments = labels.map(label => LineDataSet(label = label))
  val chart = Chart(sel).Line(LineData(js.Array(labels: _*), datasets = js.Array(segments: _*)))

  val legend = $.parseHTML(chart.generateLegend())

  sel.parent().append(chart.generateLegend())

  var recorded = 0

  def update(values: Double*): Unit = {
    while(recorded > max) {
      chart.removeData()
      recorded -= 1
    }

    //chart.datasets(0).points.push(Point(value))
    chart.addData(js.Array(values: _*), "")
    //chart.addData(res.toJsArray, "")
    //chart.update()

    recorded += 1
    //recorded += res.length
  }
}