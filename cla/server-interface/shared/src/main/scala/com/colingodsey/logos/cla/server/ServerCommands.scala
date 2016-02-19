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

  case class ThalamicInput(input: CLA.Input) extends RegionCommand
  object ThalamicInput {
    implicit val acc = ObjectAccessor.of[ThalamicInput]
  }

  case class SensoryInput(input: CLA.Input) extends RegionCommand
  object SensoryInput {
    implicit val acc = ObjectAccessor.of[SensoryInput]
  }

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

  case class RunStats(
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
  object RunStats {
    implicit val acc = ObjectAccessor.of[RunStats]
  }

  case class GetColumnView(layer: Layer)
  object GetColumnView {
    implicit val acc = ObjectAccessor.of[GetColumnView]
  }

  case object GetStats
  case object StartAuto
  case object StopAuto

  object ColumnView {
    case class Cell(active: Boolean, longActive: Boolean)
    case class Column(cells: Seq[Cell], active: Boolean, wasPredicted: Boolean)
    case class Data(columns: Seq[Column])

    implicit val cellAcc = ObjectAccessor.of[Cell]
    implicit val columnAcc = ObjectAccessor.of[Column]
    implicit val dataAcc = ObjectAccessor.of[Data]
  }

  object SineGame {
    case class Start(regionWidth: Int)
    case class NeedDataPoints(n: Int)
    case class DataPoints(x: Seq[Double])

    case object RunOne
    case object ClearPoints

    def register(): Unit = {
      registry.add(ObjectAccessor.of[Start],
        ObjectAccessor.of[NeedDataPoints],
        ObjectAccessor.of[RunStats],
        ObjectAccessor.of[DataPoints])
      registry.add(RunOne)
      registry.add(ClearPoints)
    }
  }

  implicit val vec2Acc = ObjectAccessor.of[Vec2]

  object GoalGame {
    case class Start(regionWidth: Int)

    case class ActorPosition(pos: Vec2, dir: Double)
    case class TActorPosition(pos: Vec2, dir: Double)

    case object RunOne
    case object Bump
    case object ToggleCorticalControl

    def register(): Unit = {
      registry.add(
        ObjectAccessor.of[ActorPosition],
        ObjectAccessor.of[TActorPosition],
        ObjectAccessor.of[Start]
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
