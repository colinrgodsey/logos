package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.CLA
import com.colingodsey.logos.cla.encoders.ScalarEncoder
import utest._

import com.colingodsey.logos.collections.ExtraMath.randomNormal

object CLATest extends TestSuite {

  implicit val config = CLA.DefaultConfig.copy(
    columnHeight = 32,
    //segmentThreshold = 8, seededDistalConnections = 20, maxDistalDendrites = 32,
    segmentThreshold = 8,
    dutyAverageFrames = 100,
    inputWidth = 80, inputConnectionsPerColumn = 30,
    desiredLocalActivity = 10,
    regionWidth = 256, minOverlap = 5)

  val sinSteps = 30
  val sinSteps2 = 70
  val sinSteps3 = 130

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
      val region = new L3Region

      for(i <- 0 until 10000) {
        val t = (i % sinSteps) / sinSteps.toDouble
        val t2 = (i % sinSteps2) / sinSteps2.toDouble
        val t3 = (i % sinSteps3) / sinSteps3.toDouble

        val s = math.sin(t * math.Pi * 2) / 2.0 + 0.5
        val s2 = math.sin(t2 * math.Pi * 2) / 2.0 + 0.5
        val s3 = math.sin(t3 * math.Pi * 2) / 2.0 + 0.5

        //val r = s * s2
        val r = if(i < 5000 || true) (s + s2) / 2.0
        else (s + s2 + s3) / 3.0

        region.update(encoder.encode(r))
        println("\t\t\t" + encoder.encode(r))
        println(region.anomalyScore)

      }
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