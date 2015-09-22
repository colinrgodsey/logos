package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.traits.{SDR, SpatialPooler}
import com.colingodsey.logos.collections.RollingAverage

//TODO: concat segments? can form SDRs of 2 sets of inputs, and concat segments
//TODO: calculate if this has ever fired, drop if not
final class DendriteSegment(
      protected var connections: IndexedSeq[NodeAndPermanence] = IndexedSeq.empty,
      activationThresholdOpt: Option[Int] = None)(
      implicit val config: CLA.Config[_])
    extends SDR with NeuralNode.Mutable with NeuralNode.ActivationOrdinal with NeuralNode.Ordinal {
  import config._

  var wasActive = false
  var active = false
  var activation = 0
  var potentialActivation = 0
  var receptive = 0

  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random) += math.random
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random) += math.random
  //var sequenceSegment = false

  var ordinal = math.random

  val activationThreshold: Int = activationThresholdOpt getOrElse config.segmentThreshold

  def receptiveRadius[L](implicit cfg: CLA.Config[L]): Double = {
    val topology = cfg.topology

    val locations = connections.iterator.flatMap {
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
  def activationOrdinal: (Double, Double, Double) = (overlap, 0.0/*ghostActivation*/, ordinal)

  def ghostActivation = activation.toDouble + potentialActivation.toDouble

  def isFull = numConnections >= config.seededDistalConnections

  def activateIfAbove(min: Double): Unit = {
    active = {
      val o = overlap
      o > min && o > 0
    }
  }

  def activate(): Unit = active = true
  def deactivate(): Unit = active = false

  def update(): Unit = {
    var act = 0
    var rec = 0
    var potAct = 0
    var i = 0

    val l = connections.length

    while(i < l) {
      val np = connections(i)
      val node = np.node
      val p = np.p

      val con = p > connectionThreshold

      if(con) rec += 1
      else if(node.active) potAct += 1

      if(con && node.active) act += 1

      i += 1
    }

    activation = act
    receptive = rec
    potentialActivation = potAct

    wasActive = active
    active = activation >= activationThreshold

    ordinal = math.random

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
