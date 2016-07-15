package com.colingodsey.logos.cla

import akka.actor._
import com.colingodsey.logos.cla.server.ServerCommands._

object BaseGame {

}

trait BaseGame { _: Actor with ActorLogging =>
  def region: FullRegion[_]

  def resume(): Unit

  var autoRunEnabled = true
  var ticks = 0

  var l3scoreBuffer = List[Double]()
  var l4scoreBuffer = List[Double]()
  var l5scoreBuffer = List[Double]()
  var l6scoreBuffer = List[Double]()

  def updateStats(): Unit = {
    l3scoreBuffer = region.l3Layer.anomalyScore +: l3scoreBuffer
    l4scoreBuffer = region.l4Layer.anomalyScore +: l4scoreBuffer
    l5scoreBuffer = region.l5Layer.anomalyScore +: l5scoreBuffer
    l6scoreBuffer = region.l6Layer.anomalyScore +: l6scoreBuffer

    /*l3scoreBuffer = region.l3Layer.inputLayer.inhibitionRadius +: l3scoreBuffer
    l4scoreBuffer = region.l4Layer.inputLayer.inhibitionRadius +: l4scoreBuffer
    l5scoreBuffer = region.l5Layer.inputLayer.inhibitionRadius +: l5scoreBuffer
    l6scoreBuffer = region.l6Layer.inputLayer.inhibitionRadius +: l6scoreBuffer*/
  }

  def getL3ColumnView = {
    val columns = region.l3Layer.columns map { column =>
      val cells = column.cells map { cell =>
        ColumnView.Cell(cell.active, cell.activeForTicks > 1)
      }
      ColumnView.Column(cells, active = column.active,
        wasPredicted = column.wasPredicted)
    }

    ColumnView.Data(columns)
  }

  def getInterestingInput: InputSDR[_] = region.motorInput

  def getUIStats = {
    val l3score = if(l3scoreBuffer.isEmpty) 0 else l3scoreBuffer.sum / l3scoreBuffer.length
    val l4score = if(l4scoreBuffer.isEmpty) 0 else l4scoreBuffer.sum / l4scoreBuffer.length
    val l5score = if(l5scoreBuffer.isEmpty) 0 else l5scoreBuffer.sum / l5scoreBuffer.length
    val l6score = if(l6scoreBuffer.isEmpty) 0 else l6scoreBuffer.sum / l6scoreBuffer.length

    l3scoreBuffer = Nil
    l4scoreBuffer = Nil
    l5scoreBuffer = Nil
    l6scoreBuffer = Nil

    RunStats(
      l3anomalyScore = l3score,
      l4anomalyScore = l4score,
      l5anomalyScore = l5score,
      l6anomalyScore = l6score,
      //activeDuties = region.l3Layer.inputLayer.segments.map(_.activeDutyCycle.toDouble),
      //activeDuties = region.sensoryInput.segments.map(_.activeDutyCycle.toDouble),
      activeDuties = getInterestingInput.segments.map {
        case x if x.active => x.overlap
        case x => 0.0
      },
      l3overlapDuties = region.l3Layer.inputLayer.segments.map {
        case x if x.active => x.overlap
        case x => 0.0
      },
      l4overlapDuties = region.l4Layer.inputLayer.segments.map {
        case x if x.active => x.overlap
        case x => 0.0
      },
      l5overlapDuties = region.l5Layer.inputLayer.segments.map {
        case x if x.active => x.overlap
        case x => 0.0
      },
      l6overlapDuties = region.l6Layer.inputLayer.segments.map {
        case x if x.active => x.overlap
        case x => 0.0
      },
      ticks = ticks
    )
  }

  def baseGameReceive: Receive = {
    case GetStats => sender ! getUIStats
    case StartAuto =>
      autoRunEnabled = true
      resume()
      log.info("auto run enabled")
    case StopAuto =>
      autoRunEnabled = false
      log.info("auto run disabled")

    case GetColumnView(Layer.L3) =>
      sender ! getL3ColumnView
  }
}