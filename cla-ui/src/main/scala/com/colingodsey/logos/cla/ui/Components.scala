package com.colingodsey.logos.cla.ui

import com.colingodsey.logos.collections.Vec3
import org.scalajs.dom.raw.WebSocket
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

class ColumnPolarComponent(parentSel: JQuery, width: Int, size: Int) {
  import DOMExt._
  import ChartJS._

  private implicit def ec = JSExecutionContext.queue

  val sets = (0 until width).map { i =>
    val s = i
    PolarDataSet(0.0, color = randomColor(s), highlight = randomColor(s + 1), label = i.toString)
  }.toIndexedSeq

  val canvas = s"""<canvas width="$size" height="$size"></canvas>"""
  val canvasSel = {
    parentSel.append(canvas)
    parentSel.find("canvas")
  }

  val chart = Chart(canvasSel).PolarArea(sets.toJsArray, Chart.PolarDefaults)

  def update(values: IndexedSeq[Double]): Unit = Future {
    require(values.length == width)

    (0 until width).map { i =>
      val set = chart.segments(i)

      set.value = values(i)
    }

    chart.update()
  }

  def destroy() = {
    canvasSel.remove()
    chart.destroy()
  }
}

class ColumnLineComponent(sel: JQuery, labels: Seq[String], max: Int = 32) {
  import DOMExt._
  import ChartJS._

  val segments = labels.map(label => LineDataSet(label = label))
  val chart = Chart(sel).Line(LineData(js.Array(labels: _*), datasets = js.Array(segments: _*)))

  val legend = $.parseHTML(chart.generateLegend())

  var recorded = 0

  sel.parent().append(chart.generateLegend())

  def destroy() = {
    sel.parent().find(".line-legend").remove()
    chart.destroy()
  }

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


