package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.DutyCycle.Booster
import com.colingodsey.logos.collections.RollingAverage

//TODO: calculate if this has ever fired, drop if not
final class DendriteSegment(
    val loc: CLA.Location,
    val parent: DutyCycle.Booster,
    synapses: IndexedSeq[NodeAndPermanence] = IndexedSeq.empty)(
    implicit val config: CLA.Config) extends SDR {
  import config._

  var active = false
  var activation = 0
  var potentialActivation = 0
  var receptive = 0

  val activeDutyCycle = RollingAverage(dutyAverageFrames)
  val overlapDutyCycle = RollingAverage(dutyAverageFrames)
  //var sequenceSegment = false

  val ordinal = math.random

  protected var connections = synapses.toArray

  def receptiveRadius = {
    connections.iterator.map {
      case np if np.p > connectionThreshold =>
        math.abs(np.node.loc - loc)
      case _ => 0
    }.max
  }

  //TODO: min activation for boost?
  //def activationOrdinal = (overlap, activation, potentialActivation, ordinal)
  def activationOrdinal = (overlap, activation.toDouble + potentialActivation.toDouble, 0.0, ordinal)

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

    active = activation >= activationThreshold

    if(active) boost = 0.0

    updateDutyCycle()
  }

  def dutyCycleUpdateRatio = config.segmentDutyCycleRatio
  def connectionThreshold: Double = config.connectionThreshold
  def minDistalPermanence: Double = config.minDistalPermanence
  def permanenceInc: Double = config.permanenceInc
  def permanenceDec: Double = config.permanenceDec
  def boostIncr: Double = config.boostIncr
  def activationThreshold: Int = config.segmentThreshold

  override def minThreshold = activationThreshold
}
