package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.encoders.ScalarEncoder
import utest._

import com.colingodsey.logos.collections.ExtraMath.randomNormal

object CLATest extends TestSuite {

  implicit val config = CLA.DefaultConfig.copy(inputWidth = 80,
    desiredLocalActivity = 40,
    segmentThreshold = 8, seededDistalConnections = 20, maxDistalDendrites = 128,
    columnHeight = 20,
    regionWidth = 256, minOverlap = 5, inputConnectionsPerColumn = 30)
/*
  implicit val config = CLA.DefaultConfig.copy(
    columnHeight = 20,
    regionWidth = 512
  )*/

  val tests = TestSuite {
    val encoder = new ScalarEncoder(config.inputWidth, 8)

    "run test" - {
      val region = new Region

      region.seedDistalSynapses()

      for(_ <- 0 until 1000) {
        //println(randomNormal(0.5))
        region.update(encoder.encode(0))
        println(region.anomalyScore)
        region.update(encoder.encode(0.25))
        println(region.anomalyScore)
        region.update(encoder.encode(0.5))
        println(region.anomalyScore)
        region.update(encoder.encode(1))
        println(region.anomalyScore)
        //println(region.numActive)

        region.update(encoder.encode(0))
        println(region.anomalyScore)
        region.update(encoder.encode(0.75))
        println(region.anomalyScore)
        region.update(encoder.encode(0.5))
        println(region.anomalyScore)
        region.update(encoder.encode(1))
        println(region.anomalyScore)

        val r = (math.random * 4).toInt + 1
        //region.update(encoder.encode(r / 4.0))
      }

      //region.update(encoder.encode(math.random))
      region.update(encoder.encode(0))
      region.update(encoder.encode(0.25))
      println(region.anomalyScore)
      println(region.columns.filter(_.active).mkString("\n"))
      println(region.columns.map(_.boost))
      println(region.columns.map(_.overlap))

      region.update(encoder.encode(0.5))
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