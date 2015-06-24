package com.colingodsey.logos.cla

import com.colingodsey.logos.collections.RollingAverage

import scala.concurrent.ExecutionContext

class InputSDR[L](implicit val config: CLA.Config[L],
    ec: ExecutionContext) extends DutyCycle.Booster { inputLayer =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  val segments = (for(i <- 0 until numColumns) yield createSegment(i)).toArray
  val currentInput = Array.fill(inputWidth)(false)
  val inhibitionRadiusAverage = RollingAverage(dutyAverageFrames) += regionWidth

  var maxDutyCycle = 1.0

  protected def createSegment(idx: Int) = {
    val columnLoc = topology.columnLocationFor(idx)
    val inputColumnLoc = topology.scale(columnLoc, inputWidth / numColumns.toDouble)

    val inputMap: IndexedSeq[Int] =
      topology.uniqueNormalizedLocations(inputColumnLoc, inputRangeRadius).
        take(inputConnectionsPerColumn).
        map(topology.indexFor(_, inputWidth)).toArray.toIndexedSeq

    val nodes = inputMap.map { iidx =>
      val scaledIdx = (iidx.toDouble / inputWidth * numColumns).toInt
      val node: NeuralNode = new topology.LocalNeuralNode {
        val loc: L = topology.columnLocationFor(scaledIdx)
        def active: Boolean = currentInput(iidx)
      }

      new NodeAndPermanence(node, getRandomProximalPermanence)
    }.toArray.toIndexedSeq

    new DendriteSegment(inputLayer, nodes, activationThresholdOpt = Some(minOverlap))
  }

  def columnsNear(loc: topology.Location, rad: Double) =
    topology.columnIndexesNear(loc, rad) map segments

  def update(input: Input): Unit = {
    for(i <- 0 until input.length) this.currentInput(i) = input(i)

    spatialPooler()

    inhibitionRadiusAverage += averageReceptiveFieldRadius * dynamicInhibitionRadiusScale
  }

  def inhibitionRadius: Double = math.max(inhibitionRadiusAverage.toDouble, 3)

  def averageReceptiveFieldRadius = {
    var s = 0.0
    var nActive = 0
    for(i <- 0 until segments.length) {
      val seg = segments(i)
      val loc = topology.columnLocationFor(i)

      //TODO: filter on active or not?
      if(seg.active) {
        nActive += 1
        s += seg.receptiveRadius(loc)
      }
    }

    if(nActive == 0) 0.0
    else s / nActive
  }

  def neighborsIn(loc: topology.Location, radius: Double) = columnsNear(loc, radius)

  def localSpatialPooler(segment: DendriteSegment, loc: topology.Location): Unit =
    if(segment.overlap > 0) {
      val sorted = neighborsIn(loc, inhibitionRadius).toStream.
          map(_.overlap).filter(_ > 0).sortBy(-_)

      val min = if(sorted.isEmpty) 0
      else sorted.take(desiredLocalActivity).min

      segment.active = {
        val o = segment.overlap
        o >= min && o > 0
      }
    } else segment.active = false

  protected def spatialPooler(): Unit = {
    //clear activation state and update input
    segments.foreach(_.update())

    if(dynamicInhibitionRadius && inhibitionRadius < (regionWidth / 2)) {
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
