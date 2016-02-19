package com.colingodsey.logos.cla.ui

import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.jquery.JQuery

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

//TODO: redo with js.Object style class tree with objects
object ChartJS {
  import com.colingodsey.logos.cla.ui.DOMExt._

  final class Chart(context2d: js.Dynamic) extends js.Any {
    def Line(data: LineData, config: js.Dynamic): Line = js.native
    def Line(data: LineData): Line = js.native

    def PolarArea(data: PolarData, config: js.Any): PolarArea = js.native
    def PolarArea(data: PolarData): PolarArea = js.native
  }

  object Chart {
    private val jsObject = js.Dynamic.global.Chart
    private var curAnimCompletePromise = Promise[Unit]()

    def apply(sel: JQuery): Chart = {
      require(sel.length > 0, "empty jquery selection!")

      val ctx = sel.get(0).asInstanceOf[HTMLCanvasElement].getContext("2d")
      val config = PolarDefaults

      js.Dynamic.newInstance(jsObject)(ctx, config).asInstanceOf[Chart]
    }

    trait GlobalDefaults extends js.Any {
      var animation: Boolean = js.native
      var animationSteps: Int = js.native
      var animationEasing: String = js.native
      var datasetFill: Boolean = js.native
      private[ChartJS] var onAnimationComplete: js.Function0[Unit] = js.native
    }

    val GlobalDefaults = jsObject.defaults.global.asInstanceOf[GlobalDefaults]
    val PolarDefaults = jsObject.defaults.PolarArea.asInstanceOf[GlobalDefaults]
    val LineDefaults = jsObject.defaults.Line.asInstanceOf[GlobalDefaults]

    GlobalDefaults.onAnimationComplete = { () =>
      curAnimCompletePromise.success()
      curAnimCompletePromise = Promise()
    }

    def onAnimationComplete(f: => Unit)(implicit ec: ExecutionContext): Future[Unit] = {
      for {
        _ <- timerFuture(10.millis)
        _ = f
        _ <- curAnimCompletePromise.future
      } yield ()
    }

  }

  sealed trait LineSegment extends js.Any {
    var points: js.Array[MutablePoint] = js.native
    val label: String = js.native
  }

  final class MutablePoint extends js.Any {
    var value: Double = js.native
  }

  @JSExportAll
  case class Point(value: Double)

  final class Line extends js.Any {
    var datasets: js.Array[LineSegment] = js.native
    def update(): Unit = js.native
    def destroy(): Unit = js.native
    def addData(values: js.Array[Double], label: String): Unit = js.native
    def removeData(): Unit = js.native
    def generateLegend(): String = js.native
  }

  @JSExportAll
  case class LineDataSet(
      label: String,
      data: js.Array[Double] = js.Array(),
      fillColor: js.UndefOr[Color] = randomColor: Color,
      //strokeColor: js.UndefOr[Color] = randomColor: Color,
      //pointColor: js.UndefOr[Color] = randomColor: Color,
      //pointStrokeColor: js.UndefOr[Color] = randomColor: Color,
      pointHighlightFill: js.UndefOr[Color] = randomColor: Color,
      pointHighlightStroke: js.UndefOr[Color] = randomColor: Color
  ) {
    def pointColor: js.UndefOr[Color] = fillColor
    def strokeColor: js.UndefOr[Color] = fillColor
    def pointStrokeColor: js.UndefOr[Color] = fillColor
  }

  @JSExportAll
  case class LineData(labels: js.Array[String], datasets: js.Array[LineDataSet])

  sealed trait PolarAreaSegment extends js.Any {
    var value: Double = js.native
    val label: String = js.native
  }

  final class PolarArea extends js.Any {
    var segments: js.Array[PolarAreaSegment] = js.native
    def update(): Unit = js.native
    def destroy(): Unit = js.native
  }

  type PolarData = js.Array[PolarDataSet]

  @JSExportAll
  case class PolarDataSet(
      value: Double,
      color: Color,
      highlight: Color,
      label: String
  )
}
