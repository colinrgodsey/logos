package com.colingodsey.logos.cla.ui

import org.scalajs.jquery.JQuery

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportAll

object Flot {
  import DOMExt._

  type Position = String
  object Position {
    val NE = "ne"
    val NW = "nw"
    val SE = "se"
    val SW = "sw"
  }

  type Sorting = js.Dynamic
  object Sorting {
    val True = true.asInstanceOf[Sorting]
    val False = false.asInstanceOf[Sorting]
    val Ascending = "ascending".asInstanceOf[Sorting]
    val Descending = "descending".asInstanceOf[Sorting]
    val Reverse = "reverse".asInstanceOf[Sorting]

    def apply(f: js.Dynamic => Double): Sorting =
      (f: js.Function1[js.Dynamic, Double]).asInstanceOf[Sorting]
  }

  type Data = js.Array[js.Array[js.Array[Double]]]
  object Data {
    def apply(series: Seq[(Double, Double)]*): Data = {
      series.iterator.map { seq =>
        seq.iterator.map(pair => js.Array[Double](pair._1, pair._2)).toJsArray
      }.toJsArray
    }
  }


  @JSExportAll
  case class Legend(
      show: js.UndefOr[Boolean] = js.undefined,
      labelFormatter: js.Dynamic = null,// or (fn: string, series object -> string)
      labelBoxBorderColor: Color = null,
      noColumns: js.UndefOr[Int] = js.undefined,
      position: Position = null,
      margin: js.Dynamic = null, //number of pixels or [x margin, y margin]
      backgroundColor: Color = null,
      backgroundOpacity: js.UndefOr[Double] = js.undefined,// between 0 and 1
      container: js.Dynamic = null,// or jQuery object/DOM element/jQuery expression
      sorted: Sorting = null
  )

  @JSExportAll
  case class Options(
      width: js.UndefOr[Int] = js.undefined,
      height: js.UndefOr[Int] = js.undefined,
      legend: js.UndefOr[Legend] = js.undefined,
      xaxis: js.UndefOr[Axis] = js.undefined,
      yaxis: js.UndefOr[Axis] = js.undefined
  )

  @JSExportAll
  case class Axis(
      show: Boolean = true,//null or true/false
      position: js.UndefOr[String] = js.undefined,//"bottom" or "top" or "left" or "right"
      mode: js.UndefOr[String] = js.undefined,// or "time" ("time" requires jquery.flot.time.js plugin)
      timezone: js.UndefOr[String] = js.undefined, // "browser" or timezone (only makes sense for mode: "time")

      color: Color = null,
      tickColor: Color = null,
      font: js.Dynamic = null,// or font spec object

      min: js.Dynamic = null,// or number
      max: js.Dynamic = null,// or number
      autoscaleMargin: js.Dynamic = null,// or number

      transform: js.Dynamic = null,// or fn: number -> number
      inverseTransform: js.Dynamic = null,// or fn: number -> number

      ticks: js.Dynamic = null,// null or number or ticks array or (fn: axis -> ticks array)
      tickSize: js.Dynamic = null,// number or array
      minTickSize: js.Dynamic = null, //number or array
      tickFormatter: js.Dynamic = null,// (fn: number, object -> string) or string
      tickDecimals: js.Dynamic = null, //null or number

      labelWidth: js.Dynamic = null,// null or number
      labelHeight: js.Dynamic = null,// null or number
      reserveSpace: js.Dynamic = null,// null or true

      tickLength: js.Dynamic = null,// null or number

      alignTicksWithAxis: js.Dynamic = null// null or number
  )
}

trait Flot extends js.Object {
  import Flot._

  def plot(data: Data, options: Options): FlotPlot = js.native
}

sealed trait FlotPlot extends js.Object {
  var data: Flot.Data = js.native
}

trait FlotStatic extends js.Object {
  import Flot._

  def plot(q: JQuery, data: Data, options: Options): FlotPlot = js.native
}