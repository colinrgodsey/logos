package com.colingodsey.logos.cla

import scala.concurrent.ExecutionContext

class InputSDR(implicit val config: CLA.Config, ec: ExecutionContext) extends Layer { inputLayer =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  val segments = (for(i <- 0 until regionWidth) yield createSegment(i)).toArray
  val currentInput = Array.fill(inputWidth)(false)

  var maxDutyCycle = 1.0

  protected def createSegment(loc: Location) = {
    val _loc = loc

    val inputMap: IndexedSeq[Int] = {
      var outSet = Set[Int]()
      var out = Seq[Int]()
      var added = 0

      val inputLoc = loc / regionWidth * inputWidth

      while(added < inputConnectionsPerColumn) {
        val i = (math.random * inputWidth).toInt
        //val i = (randomNormal(0.1) * inputWidth + inputLoc).toInt

        if(i >= 0 && i < inputWidth && !outSet(i)) {
          outSet += i
          out :+= i
          added += 1
        }
      }

      out.toArray
    }

    val nodes = (0 until inputConnectionsPerColumn map { idx =>
      val node: NeuralNode = new NeuralNode {
        val loc: CLA.Location = _loc
        def active: Boolean = currentInput(inputMap(idx))
      }

      new NodeAndPermanence(node, getRandomProximalPermanence)
    }).toArray.toIndexedSeq

    new DendriteSegment(loc, inputLayer, nodes, activationThresholdOpt = Some(minOverlap))
  }

  def update(input: Input): Unit = {
    for(i <- 0 until input.length) this.currentInput(i) = input(i)

    spatialPooler()
  }

  protected def spatialPooler(): IndexedSeq[DendriteSegment] = {
    //clear activation state and update input
    segments.foreach(_.update())

    //TODO: real inhibition radius? or no?
    val sorted = segments.sortBy { segment =>
      (-segment.overlap, -segment.activation, segment.ordinal)
    }
    val (topK, tail) = sorted.splitAt(desiredLocalActivity)

    //activated top columns within our inhibition radius
    tail.foreach(_.active = false)
    topK.foreach(x => x.active = x.overlap > 0) //only active inputs

    /*
    columns.foreach(_.spatialPooler())
*/

    //update rolling averages
    //columns.foreach(_.updateDutyCycle())
    VM.distributedExec(regionWidth / numWorkers, segments)(_.updateDutyCycle(force = true))

    maxDutyCycle = segments.maxBy(_.activeDutyCycle.toDouble).activeDutyCycle.toDouble
    //inhibitionRadiusAverage += averageReceptiveFieldRadius / inputWidth * regionWidth

    topK
  }
}
