package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

import scala.concurrent.ExecutionContext

//TODO: use sigmoid value for overlap sum
class InputSDR[L](implicit val config: CLA.Config[L],
    ec: ExecutionContext) extends DutyCycle.Booster { inputLayer =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  val width = config.numColumns

  val segments = (for(i <- 0 until width) yield createSegment(i)).toArray
  val currentInput = Array.fill(inputWidth)(false)
  val inhibitionRadiusAverage = RollingAverage(dutyAverageFrames) += regionWidth

  var maxDutyCycle = 1.0

  protected def createSegment(idx: Int) = {
    val columnLoc = topology.columnLocationFor(idx)
    val inputColumnLoc = topology.scale(columnLoc, inputWidth / width.toDouble)

    val indexStream = if(nonLocalizedInput) {
      Stream.continually((math.random * inputWidth).toInt).take(10000).distinct
    } else {
      topology.uniqueNormalizedLocations(inputColumnLoc, config.inputRangeRadius, inputWidth).
          map(topology.indexFor(_, inputWidth))
    }

    val inputMap: IndexedSeq[Int] =
      indexStream.take(config.inputConnectionsPerColumn).toArray.toIndexedSeq

    val nodes = inputMap.map { iidx =>
      val inputLoc = topology.columnLocationFor(iidx)
      val node: NeuralNode = new topology.LocalNeuralNode {
        val loc: L = topology.scale(inputLoc, width.toDouble / inputWidth)
        def active: Boolean = currentInput(iidx)
      }

      new NodeAndPermanence(node, getRandomProximalPermanence)
    }.toArray.toIndexedSeq

    new DendriteSegment(inputLayer, nodes, activationThresholdOpt = Some(minOverlap))
  }

  def columnsNear(loc: topology.Location, rad: Double) =
    topology.columnIndexesNear(loc, rad) map segments

  def update(input: Input): Unit = {
    for(i <- 0 until math.min(input.length, inputWidth)) this.currentInput(i) = input(i)

    spatialPooler()

    inhibitionRadiusAverage += averageReceptiveFieldRadius * dynamicInhibitionRadiusScale / inputWidth * regionWidth
  }

  def inhibitionRadius: Double = math.max(inhibitionRadiusAverage.toDouble, 3)

  def averageReceptiveFieldRadius = {
    var s = 0.0
    var nActive = 0
    for(i <- 0 until segments.length) {
      val seg = segments(i)

      //TODO: filter on active or not?
      if(seg.active) {
        nActive += 1
        s += seg.receptiveRadius[topology.Location]
      }
    }

    if(nActive == 0) 0.0
    else s / nActive
  }

  def neighborsIn(loc: topology.Location, radius: Double) = columnsNear(loc, radius)

  def localSpatialPooler(segment: DendriteSegment, loc: topology.Location): Unit =
    if(segment.overlap > 0) {
      val sorted = neighborsIn(loc, inhibitionRadius).filter(_ != segment).
          map(_.overlap).filter(_ > 0).toStream.sortBy(-_)

      val min = if(sorted.isEmpty) 0
      else sorted.take(desiredLocalActivity).min

      segment.active = {
        val o = segment.overlap
        o > min && o > 0
      }
    } else segment.active = false

  protected def spatialPooler(): Unit = {
    //clear activation state and update input
    segments.foreach(_.update())
//println(inhibitionRadius)
    if(dynamicInhibitionRadius && inhibitionRadius < (inputWidth / 2)) {
      for(i <- 0 until segments.length) {
        val seg = segments(i)
        val loc = topology.columnLocationFor(i)

        localSpatialPooler(seg, loc)
      }
    } else {
      val sorted = segments.sortBy { segment =>
        (-segment.overlap, -segment.activation, segment.ordinal)
      }
      val (topK, tail) = sorted.splitAt(desiredLocalActivity)

      //activated top columns within our inhibition radius
      tail.foreach(_.active = false)
      topK.foreach(x => x.active = x.overlap > 0) //only active inputs
    }

    //update rolling averages
    VM.distributedExec(regionWidth / numWorkers, segments)(_.updateDutyCycle(force = true))

    maxDutyCycle = segments.maxBy(_.activeDutyCycle.toDouble).activeDutyCycle.toDouble
  }
}
