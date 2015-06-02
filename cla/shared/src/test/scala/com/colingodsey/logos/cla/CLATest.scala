package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.encoders.ScalarEncoder
import utest._

object CLATest extends TestSuite {

  implicit val config = CLA.DefaultConfig.copy(inputWidth = 80,
    desiredLocalActivity = 40,
    segmentThreshold = 8, seededDistalConnections = 25, maxDistalDendrites = 6,
    columnHeight = 20,
    regionWidth = 128, minOverlap = 4, inputConnectionsPerColumn = 30)

  val tests = TestSuite {
    val encoder = new ScalarEncoder(config.inputWidth, 8)

    "run test" - {
      val region = new Region

      region.seedDistalSynapses()

      for(_ <- 0 until 600) {
        //region.update(encoder.encode(math.random))
        region.update(encoder.encode(0))
        region.update(encoder.encode(0.25))
        region.update(encoder.encode(0.5))
        region.update(encoder.encode(1))
      }

      //region.update(encoder.encode(math.random))
      region.update(encoder.encode(0))
      region.update(encoder.encode(0.25))
      println(region.columns.filter(_.active).mkString("\n"))
      println(region.columns.map(_.boost))
      println(region.columns.map(_.overlap))

      region.update(encoder.encode(0.5))
      println(region.columns.filter(_.active).mkString("\n"))
      println(region.columns.map(_.boost))
      println(region.columns.map(_.overlap))
      println(region.columns.map(_.activeDutyCycle.toDouble))
    }
  }

}