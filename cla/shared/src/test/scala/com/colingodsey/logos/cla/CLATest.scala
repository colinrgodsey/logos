package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.CLA
import com.colingodsey.logos.cla.encoders.ScalarEncoder
import utest._

import com.colingodsey.logos.collections.ExtraMath.randomNormal

object CLATest extends TestSuite {

  implicit val config = CLA.ReducedConfig

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
    val encoder = new ScalarEncoder(config.inputWidth, config.minOverlap + 5)

    "run test" - {
      //val region = new L3Region
      val region = new L4Region

      for(i <- 0 until 1000) {
        val t = (i % sinSteps) / sinSteps.toDouble
        val t2 = (i % sinSteps2) / sinSteps2.toDouble
        val t3 = (i % sinSteps3) / sinSteps3.toDouble

        val s = math.sin(t * math.Pi * 2) / 2.0 + 0.5
        val s2 = math.sin(t2 * math.Pi * 2) / 2.0 + 0.5
        val s3 = math.sin(t3 * math.Pi * 2) / 2.0 + 0.5

        //val r = s * s2
        /*val r = if(i < 5000 || true) (s + s2) / 2.0
        else (s + s2 + s3) / 3.0*/
        val r = s

        region.update(encoder.encode(r))//, IndexedSeq.fill(config.inputWidth)(false))
        //println("\t\t\t" + encoder.encode(r))
        //println(region.l3Layer.anomalyScore, region.l3Layer.inhibitionRadius)
        //println(region.l3Layer.anomalyScore, region.l3Layer.inhibitionRadius, region.sensoryInput.averageReceptiveFieldRadius)

      }
      println(region.l3Layer.anomalyScore)
      println(region.l3Layer.columns.filter(_.active).mkString("\n"))
      println(region.l3Layer.columns.map(_.boost))
      println(region.l3Layer.columns.map(_.overlap))

      region.update(encoder.encode(math.random))//, IndexedSeq.fill(config.inputWidth)(false))
      println(region.l3Layer.anomalyScore)
      println(region.l3Layer.columns.filter(_.active).mkString("\n"))
      println(region.l3Layer.columns.map(_.boost))
      println(region.l3Layer.columns.map(_.overlap))
      //println(region.l3Layer.columns.map(_.activeDutyCycle.toDouble))
      //println(region.l3Layer.columns.map(_.overlapDutyCycle.toDouble))
      //println(region.l3Layer.inhibitionRadius)
      println(region.sensoryInput.averageReceptiveFieldRadius)
    }
  }

}