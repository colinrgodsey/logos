package com.colingodsey.logos.cla.server

import com.colingodsey.logos.cla.CLA
import com.colingodsey.logos.collections.Vec2
import json._
import json.tools.AccessorRegistry
import json.tools.Enumerator

object ServerCommands {
  object registry extends AccessorRegistry

  case object UIIdentity
  case object GameIdentity

  sealed trait Command
  sealed trait Response

  sealed trait RegionCommand extends Command
  sealed trait RegionResponse extends Response

  @accessor case class ThalamicInput(input: CLA.Input) extends RegionCommand

  @accessor case class SensoryInput(input: CLA.Input) extends RegionCommand

  sealed trait Layer extends Layer.Value {
    def key: String = toString.toLowerCase
  }
  object Layer extends Enumerator[Layer] {
    case object L1 extends Layer
    case object L3 extends Layer
    case object L4 extends Layer
    case object L5 extends Layer
    case object L6 extends Layer

    val values = Set[Layer](L1, L3, L4, L5, L6)
  }

  @accessor case class RunStats(
      l3anomalyScore: Double,
      l4anomalyScore: Double,
      l5anomalyScore: Double,
      l6anomalyScore: Double,
      activeDuties: IndexedSeq[Double],
      l3overlapDuties: IndexedSeq[Double],
      l4overlapDuties: IndexedSeq[Double],
      l5overlapDuties: IndexedSeq[Double],
      l6overlapDuties: IndexedSeq[Double],
      ticks: Int)

  @accessor case class GetColumnView(layer: Layer)

  case object GetStats
  case object StartAuto
  case object StopAuto

  object ColumnView {
    @accessor case class Cell(active: Boolean, longActive: Boolean)
    @accessor case class Column(cells: Seq[Cell], active: Boolean, wasPredicted: Boolean)
    @accessor case class Data(columns: Seq[Column])
  }

  object SineGame {
    @accessor case class Start(regionWidth: Int)
    @accessor case class NeedDataPoints(n: Int)
    @accessor case class DataPoints(x: Seq[Double])

    case object RunOne
    case object ClearPoints

    def register(): Unit = {
      registry.add(accessorOf[Start],
        accessorOf[NeedDataPoints],
        accessorOf[RunStats],
        accessorOf[DataPoints])
      registry.add(RunOne)
      registry.add(ClearPoints)
    }
  }

  implicit val vec2Acc = ObjectAccessor.create[Vec2]

  object GoalGame {
    @accessor case class Start(regionWidth: Int)

    @accessor case class ActorPosition(pos: Vec2, dir: Double)
    @accessor case class TActorPosition(pos: Vec2, dir: Double)

    case object RunOne
    case object Bump
    case object ToggleCorticalControl

    def register(): Unit = {
      registry.add(
        accessorOf[ActorPosition],
        accessorOf[TActorPosition],
        accessorOf[Start]
      )
      registry.add(RunOne)
      registry.add(Bump)
      registry.add(ToggleCorticalControl)
    }
  }

  def register(): Unit = {
    registry.add[ThalamicInput]
    registry.add[SensoryInput]
    registry.add[GetColumnView]
    registry.add[RunStats]
    registry.add(GameIdentity)
    registry.add(UIIdentity)
    registry.add(GetStats)
    registry.add(StartAuto)
    registry.add(StopAuto)

    registry.add[ColumnView.Data]

    SineGame.register()
    GoalGame.register()

    AccessorRegistry.add(registry)
  }

  register()
}
