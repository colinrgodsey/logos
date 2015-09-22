package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.SpatialPooler
import com.colingodsey.logos.collections.RollingAverage

import scala.concurrent.ExecutionContext

//TODO: use sigmoid value for overlap sum
class InputSDR[L](implicit val config: CLA.Config[L]) extends SpatialPooler[L] with CLA.InputSource { inputLayer =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  def this(source: CLA.InputSource)(implicit config: CLA.Config[L]) {
    this()(config.copy(inputWidth = source.width))
  }

  val segments: IndexedSeq[DendriteSegment] =
    (for(i <- 0 until numColumns) yield createSegment(i)).toArray[DendriteSegment]
  val inhibitionRadiusAverage = RollingAverage(dutyAverageFrames) += regionWidth

  var currentInput: IndexedSeq[Boolean] = Array.fill(inputWidth)(false)
  var maxDutyCycle = 1.0

  protected def createSegment(idx: Int): DendriteSegment = {
    val columnLoc = topology.columnLocationFor(idx)
    val inputColumnLoc = topology.scale(columnLoc, inputWidth / numColumns.toDouble)

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
        val loc: L = topology.scale(inputLoc, numColumns.toDouble / inputWidth)
        def active: Boolean = currentInput(iidx)
      }

      new NodeAndPermanence(node, getRandomProximalPermanence)
    }.toArray

    new DendriteSegment(nodes, activationThresholdOpt = Some(minOverlap))
  }

  def columnsNear(loc: topology.Location, rad: Double) =
    topology.columnIndexesNear(loc, rad, regionWidth) map segments

  def update(layer: Layer): Unit = update(layer.produce)

  def update(input: Input): Unit = {
    require(input.length == inputWidth,
      s"expected input width ${inputWidth} got ${input.length}")
    this.currentInput = input

    segments.foreach(_.update())
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

  def neighborsIn(loc: topology.Location, radius: Double): Iterator[DendriteSegment] =
    columnsNear(loc, radius)

  def width = config.numColumns

  def iterator = segments.iterator.map(_.active)
}
