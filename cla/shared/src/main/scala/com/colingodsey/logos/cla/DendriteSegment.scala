package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.DutyCycle.Booster
import com.colingodsey.logos.collections.RollingAverage

//TODO: calculate if this has ever fired, drop if not
final class DendriteSegment(
    val parent: DutyCycle.Booster,
    synapses: IndexedSeq[NodeAndPermanence] = IndexedSeq.empty,
    activationThresholdOpt: Option[Int] = None)(
    implicit val config: CLA.Config[_]) extends SDR {
  import config._

  var wasActive = false
  var active = false
  var activation = 0
  var potentialActivation = 0
  var receptive = 0

  val activeDutyCycle = RollingAverage(dutyAverageFrames, math.random) += 1.0
  val overlapDutyCycle = RollingAverage(dutyAverageFrames, math.random) += 1.0
  //var sequenceSegment = false

  var ordinal = math.random

  val activationThreshold: Int = activationThresholdOpt getOrElse config.segmentThreshold

  protected var connections = synapses.toArray

  def receptiveRadius[L](center: L)(implicit cfg: CLA.Config[L]): Double = {
    val topology = cfg.topology
    connections.iterator.map {
      case NodeAndPermanence(node: topology.LocalNeuralNode, p) if p > connectionThreshold =>
        topology.distance(center, node.loc)
      case NodeAndPermanence(node: topology.LocalNeuralNode, _) =>
        0.0
      case x => sys.error("unexpected non-locale synpase " + x)
    }.max
  }

  /*
  TODO: fundamental issue with ordinal

    * similar cells are selected as the learning cells constantly
    * should somehow also maybe reinforce similar connections instead of just over binding?
    * results in first order sequences, not variable order
    *
    * maybe reinforce the most active, but if bursting, learningcell = random
   */

  /*

  as a 'tie-breaker', tend to favor the least active segments
  (produce new sequences instead of converging on a recently active one)

  if we cant find the most active segment, we find the most potentially active one
   */
  def activationOrdinal = (if(active) overlap else 0.0, 0.0/*ghostActivation*/, 0.0, ordinal)

  def ghostActivation = activation.toDouble + potentialActivation.toDouble

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

    if(active) boost = 0.0
  }

  def dutyCycleUpdateRatio = config.segmentDutyCycleRatio
  def connectionThreshold: Double = config.connectionThreshold
  def minDistalPermanence: Double = config.minDistalPermanence
  def permanenceInc: Double = config.permanenceInc
  def permanenceDec: Double = config.permanenceDec
  def boostIncr: Double = config.boostIncr

  override def minThreshold = activationThreshold
}
