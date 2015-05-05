package com.colingodsey.logos.collections

import scala.util.control.NonFatal

import utest._

object CLATest extends TestSuite {
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
      for(_ <- 0 until 100) cla.advance()
      for(_ <- 0 until 1700) cla.think()

      val out = cla.advance().normal
      val sorted = out.weights.toSeq.sortBy(-_._2)

      println(sorted)

      assert(sorted.head._1 == "car")
    }

    "learn" - {
      "and test" - {
        val a = Node("a")
        val b = Node("b")
        val a1b1 = Node("a1b1")
        val a0b0 = Node("a0b0")
        val a1b0 = Node("a1b0")
        val a0b1 = Node("a0b1")
        val t = Node("t")
        val f = Node("f")

        val cla = new CLA
        cla.nodes = Set(a, b, a1b1, a0b0, a1b0, a0b1, t, f)

        cla.shake(0.3)

        for(_ <- 0 until 60) {
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

        val out = cla.advance(VecN("a" -> 10.0, "b" -> 10.0)).normal
        val sorted = out.weights.toSeq.sortBy(-_._2)

        println(sorted)
      }
    }
  }
}