package com.colingodsey.logos.cla.ui

import com.colingodsey.logos.cla.server.ServerCommands
import com.colingodsey.logos.collections.Vec2
import json.ObjectAccessor
import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.{dom => window}
import org.scalajs.dom.{CanvasRenderingContext2D, document}

import org.scalajs.jquery.{JQueryStatic, JQuery}

import scala.scalajs.js

import js.Dynamic.global

import org.scalajs.dom.ext.{Color => DOMColor}



class ColumnView(sel: JQuery) {
  import ServerCommands.ColumnView._

  val canvas = sel(0).asInstanceOf[HTMLCanvasElement]
  val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

  val cellRadius = 5.0
  val cellSize = cellRadius * 2.0
  val columnWidth = cellSize
  val topColumnOffset = 3.0

  def drawColumn(pos: Vec2, column: Column): Unit = {

    if(column.active) {
      context.beginPath()
      context.moveTo(pos.x, pos.y)
      context.lineTo(pos.x + columnWidth, pos.y)
      context.strokeStyle = if(column.wasPredicted) "green" else "yellow"
      context.stroke()
    }
    def drawCell(center: Vec2, cell: Cell): Unit = {
      context.beginPath()
      context.arc(center.x, center.y, cellRadius, 0, 2 * math.Pi, false)
      context.fillStyle = cell.active match {
        case true if column.active => "green"
        case true if cell.longActive => "blue"
        case true => "yellow"
        //case _ if cell.predictive => "red"
        case _ => "white"
      }
      context.fill()
      context.lineWidth = 2
      context.strokeStyle = "black"
      context.stroke()
    }

    for {
      i <- 0 until column.cells.length
      cell = column.cells(i)
    } drawCell(pos + Vec2(cellSize / 2.0, (i + 1) * cellSize + topColumnOffset), cell)
  }

  def update(data: Data): Unit = {
    context.clearRect(0, 0, canvas.width, canvas.height)
    context.save()

    //context.scale(1, -1)
    //context.translate(0, -canvas.height)

    for {
      i <- 0 until data.columns.length
      column = data.columns(i)
    } drawColumn(Vec2(i * columnWidth, 1), column)

    context.restore()
  }
}
