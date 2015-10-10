package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.{DutyCycle, SDR, SpatialPooler}
import com.colingodsey.logos.collections.RollingAverage

//TODO: concat segments? can form SDRs of 2 sets of inputs, and concat segments
//TODO: calculate if this has ever fired, drop if not
final class DendriteSegment(
      val inConnections: IndexedSeq[NodeAndPermanence] = Vector.empty,
      activationThresholdOpt: Option[Int] = None)(
      implicit val config: CLA.Config[_])
    extends SDR with DutyCycle with NeuralNode.Mutable with NeuralNode.ActivationOrdinal with NeuralNode.RandomOrdinal {
  import config._

  var wasActive = false
  var activation = 0
  var receptive = 0

  private var _active = false

  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random) += 0.001
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random) += 0.001
  //var sequenceSegment = false

  var randomOrdinal = math.random

  val activationThreshold: Int = activationThresholdOpt getOrElse config.segmentThreshold

  inConnections.foreach(addConnection)

  def receptiveRadius[L](implicit cfg: CLA.Config[L]): Double = {
    val topology = cfg.topology

    val locations = connections.flatMap {
      case NodeAndPermanence(node: topology.LocalNeuralNode, p) if p > connectionThreshold =>
        Some(node.loc)
      case NodeAndPermanence(node: topology.LocalNeuralNode, _) =>
        None
      case x => sys.error("unexpected non-locale synpase " + x)
    }

    topology.radiusOfLocations(locations, cfg.regionWidth)
  }

  /*
  activation with orginal used as a tie breaker
  TODO: this is very important- can cause over convergence of sequences....
   */
  def activationOrdinal: (Double, Double, Double) = (overlap, 0.0/*ghostActivation*/, randomOrdinal)

  def isFull = numConnections >= config.seededDistalConnections

  val permanentOrdinal = math.random * 0.1

  override def overlap: Double = {
    val o = super.overlap

    if(o > 0) o + permanentOrdinal
    else 0
  }

  def activateIfAbove(min: Double): Unit = {
    _active = {
      val o = overlap
      o > min && o > 0
    }
  }

  def active = _active

  def activate(): Unit = _active = true
  def deactivate(): Unit = _active = false

  def update(): Unit = {
    var act = 0
    var rec = 0

    var i = 0

    val l = connectionBuffer.length

    while(i < l) {
      val np = connectionBuffer(i)
      if(np != null) {
        val con = np.p > connectionThreshold

        if (con) rec += 1

        if (con && np.node.active) act += 1
      }

      i += 1
    }

    activation = act
    receptive = rec

    wasActive = active
    _active = activation >= activationThreshold

    randomOrdinal = math.random

    if(active) {
      boost = 0.0
    }
  }

  def dutyCycleUpdateRatio = config.segmentDutyCycleRatio
  def connectionThreshold: Double = config.connectionThreshold
  def minDistalPermanence: Double = config.minDistalPermanence
  def permanenceInc: Double = config.permanenceInc
  def permanenceDec: Double = config.permanenceDec
  def boostIncr: Double = config.boostIncr

  override def minThreshold = activationThreshold
}
