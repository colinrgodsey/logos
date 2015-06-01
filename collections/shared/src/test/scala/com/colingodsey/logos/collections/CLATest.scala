package com.colingodsey.logos.collections

import scala.util.control.NonFatal

import utest._

object CLATest extends TestSuite {

  val config = FinalCLA.DefaultConfig.copy(inputWidth = 80,
    desiredLocalActivity = 4,
    regionWidth = 128, minOverlap = 3, inputConnectionsPerColumn = 40)

  val tests = TestSuite {
    def toBoolArray(seq: Int*) = seq.flatMap {
      case 0 => Seq.fill(6)(false)
      case _ => Seq.fill(6)(true)
    }.toArray

    val encoder = new ScalarEncoder(config.inputWidth, 9)

    "run test" - {
      val region = new Region(config)

      region.seedDistalSynapses()

      for(_ <- 0 until 300) {
        region.update(encoder.encode(0))
        region.update(encoder.encode(0.25))
        region.update(encoder.encode(0.5))
        region.update(encoder.encode(1))
      }

      println(region.columns.filter(_.active).mkString("\n"))
      println(region.columns.map(_.boost))
      println(region.columns.map(_.overlap))
    }
  }

/*
import CLA._
  val tests = TestSuite {
    "car test" - {
      val window = Node("window", VecN("glass" -> 1.0))
      val wheel = Node("wheel", VecN("round" -> 1.0))
      val seat = Node("seat", VecN("fabric" -> 1.0))

      val house = Node("house", VecN(window.id -> 1.0, seat.id -> 0.3))
      val car = Node("car", VecN(window.id -> 1.0, seat.id -> 1.0, wheel.id -> 2.0))

      val wine = Node("wine", VecN("glass" -> 2.0))

      val cla = new CLA
      cla.nodes = Set(house, car, seat, wheel, window, wine)

      for(_ <- 0 until 100) cla.advance(VecN("glass" -> 1.0, "round" -> 4.0, "fabric" -> 1.0))
      for(_ <- 0 until 100) cla.advance(VecN("glass" -> 0.7, "fabric" -> 0.9))
      for(_ <- 0 until 100) cla.advance(VecN("round" -> 1.0))
      for(_ <- 0 until 100) cla.advance(VecN("wine" -> 1.0))
      for(_ <- 0 until 100) cla.advance(VecN("glass" -> 1.0, "round" -> 4.0, "fabric" -> 1.0))
      for(_ <- 0 until 100) cla.advance()
      for(_ <- 0 until 1700) cla.think()
      for(_ <- 0 until 100) cla.advance(VecN("glass" -> 1.0, "round" -> 4.0, "fabric" -> 1.0))

      val out = cla.currentImpulse.safeNormal
      val sorted = out.weights.toSeq.sortBy(-_._2)

      println(sorted)

      //assert(sorted.head._1 == "car")
    }

    "and test" - {
      val a = Node("a")
      val b = Node("b")
      val t = Node("t")
      val f = Node("f")
    }

    "learn" - {
      "and test" - {
        val a = Node("a")
        val b = Node("b")
        val t = Node("t")
        val f = Node("f")

        val innerNodes = for(i <- 0 until 20) yield Node("_" + i)

        val cla = new CLA
        cla.nodes = Set(a, b, t, f) ++ innerNodes

        cla.shake(1)

        for(_ <- 0 until 10) {
          for (_ <- 0 until 200) cla.advance(VecN("a" -> 1.0, "b" -> 1.0, "t" -> 1.0))
          for (_ <- 0 until 50) cla.think()
          for (_ <- 0 until 200) cla.advance(VecN("a" -> 1.0, "f" -> 1.0))
          for (_ <- 0 until 50) cla.think()
          for (_ <- 0 until 100) cla.advance(VecN("a" -> 1.0, "b" -> 1.0, "t" -> 1.0))
          for (_ <- 0 until 50) cla.think()
          for (_ <- 0 until 200) cla.advance(VecN("b" -> 1.0, "f" -> 1.0))
          for (_ <- 0 until 50) cla.think()
          for (_ <- 0 until 200) cla.advance(VecN("f" -> 1.0))
          for (_ <- 0 until 50) cla.think()
        }

        for(_ <- 0 until 10) cla.advance(VecN("a" -> 10.0, "b" -> 10.0))

        val out = cla.advance(VecN("a" -> 10.0, "b" -> 10.0)).safeNormal
        val sorted = out.weights.toSeq.sortBy(-_._2).filter(x => !x._1.startsWith("_"))

        println(sorted)

        //assert(sorted(2)._1 == "a")
      }
    }
  }*/
}