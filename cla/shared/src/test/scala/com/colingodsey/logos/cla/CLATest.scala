package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.encoders.ScalarEncoder
import utest._

import com.colingodsey.logos.collections.ExtraMath.randomNormal

object CLATest extends TestSuite {

  implicit val config = CLA.DefaultConfig.copy(inputWidth = 80,
    desiredLocalActivity = 40,
    segmentThreshold = 8, //seededDistalConnections = 20, maxDistalDendrites = 32,
    columnHeight = 32,
    regionWidth = 256, minOverlap = 5, inputConnectionsPerColumn = 30)

  val sinSteps = 30
  val sinSteps2 = 10

  /*
  implicit val config = CLA.DefaultConfig.copy(
    columnHeight = 20,
    regionWidth = 512,
    inputWidth = 80,
    inputConnectionsPerColumn = 30
  )*/

  val tests = TestSuite {
    val encoder = new ScalarEncoder(config.inputWidth, 8)

    "run test" - {
      val region = new Region

      region.seedDistalSynapses()

      for(_ <- 0 until 150) {
        for(i <- 0 until sinSteps) {
          val p = i / sinSteps.toDouble * math.Pi * 2

          region.update(encoder.encode(math.sin(p) / 2.0 + 0.5))
          println("\t\t\t" + encoder.encode(math.sin(p) / 2.0 + 0.5))
          println(region.anomalyScore)
        }
      }
/*
      for(_ <- 0 until 30) {
        for(i <- 0 until sinSteps2) {
          val p = i / sinSteps2.toDouble * math.Pi * 2

          region.update(encoder.encode(math.sin(p) / 2.0 + 0.5))
          println("\t\t\t" + encoder.encode(math.sin(p) / 2.0 + 0.5))
          println(region.anomalyScore)
        }
      }*/

      println(region.anomalyScore)
      println(region.columns.filter(_.active).mkString("\n"))
      println(region.columns.map(_.boost))
      println(region.columns.map(_.overlap))

      region.update(encoder.encode(math.random))
      println(region.anomalyScore)
      println(region.columns.filter(_.active).mkString("\n"))
      println(region.columns.map(_.boost))
      println(region.columns.map(_.overlap))
      println(region.columns.map(_.activeDutyCycle.toDouble))
      println(region.columns.map(_.overlapDutyCycle.toDouble))
      println(region.inhibitionRadius)
      println(region.averageReceptiveFieldRadius)
    }
  }

}