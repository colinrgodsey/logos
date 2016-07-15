package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.SpatialPooler
import com.colingodsey.logos.collections.RollingAverage
import com.colingodsey.logos.qlearning.BoltzmannSelector

import scala.concurrent.ExecutionContext

//TODO: use boltzmann selector for input mapping
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

  def activeSegments = segments.iterator.filter(_.active)

  println("min overlap " + minOverlap)

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
      val loc: L = topology.scale(inputLoc, numColumns.toDouble / inputWidth)
      val node = new SDRInputNode(loc, iidx)

      new NodeAndPermanence(node, getRandomProximalPermanence)
    }.toArray

    new DendriteSegment(nodes, activationThresholdOpt = Some(minOverlap))
  }

  final class SDRInputNode(val loc: L, val iidx: Int) extends topology.LocalNeuralNode {
    val sdr = inputLayer

    def active: Boolean = sdr.currentInput(iidx)
  }

  def columnsNear(loc: topology.Location, rad: Double) =
    topology.columnIndexesNear(loc, rad, regionWidth) map segments

  def update(layer: CLA.InputSource): Unit = update(layer.produce)

  def update(input: Input): Unit = {
    require(input.length == inputWidth,
      s"expected input width ${inputWidth} got ${input.length}")
    currentInput = input

    segments.foreach(_.update())
    spatialPooler()
    activeSegments.foreach(_.reinforce())

    inhibitionRadiusAverage += averageReceptiveFieldRadius * dynamicInhibitionRadiusScale / inputWidth * regionWidth
  }

  def inhibitionRadius: Double = math.max(inhibitionRadiusAverage.toDouble, inhibitionRadiusMinimum)

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

  //TODO: redo this to have SDR sort everything by overlap first, then just look up scores in spatial pooler
  def neighborsIn(loc: topology.Location, radius: Double): Iterator[DendriteSegment] =
    columnsNear(loc, radius)

  def width = config.numColumns

  def iterator = segments.iterator.map(_.active)
}
