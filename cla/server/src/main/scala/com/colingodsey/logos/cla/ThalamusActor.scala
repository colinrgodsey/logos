package com.colingodsey.logos.cla

import akka.actor._
import com.colingodsey.logos.cla.CLA.{RingTopology, InputSource}

import concurrent.blocking

object ThalamusActor {
  def apply(width: Int, inWidth: Int, minDesiredActivity: Int = 10) = Props(classOf[ThalamusActor], width, inWidth, minDesiredActivity)

  case class ReceiveInput(input: CLA.Input)

  case class ThalamusOutput(input: CLA.Input)

  case class Update(reward: Double)
}

//TODO: use outputSDR as 'selected actions' and not just individual points (spatial pool over output)

//TODO: some way to eventually wire corticalInput directly to output bits?

//TODO: maxQ in qlearning is 'predicted' max Q. could that be done with the l5 output?
class ThalamusActor(outWidth: Int, inWidth: Int, minDesiredActivity: Int) extends SimpleThalamus(outWidth, minDesiredActivity) with NeuralEntityActor {
  import ThalamusActor._

  var thalamusInput: CLA.Input = new Array[Boolean](inWidth)

  val thalaSource = new InputSource {
    def iterator: Iterator[Boolean] = produce.iterator
    override def produce = thalamusInput

    def width: Bit = inWidth
  }

  val cfg = CLA.ReducedConfig.copy(regionWidth = outWidth, inputWidth = inWidth)
  val inputSDR = new InputSDR(thalaSource)(cfg.copy(
    desiredActivityPercent = 0.001,
    dynamicInhibitionRadiusScale = cfg.dynamicInhibitionRadiusScale * 5,
    inputRangeSpreadPercent = cfg.inputRangeSpreadPercent * 3,
    //desiredActivityPercent = cfg.desiredActivityPercent * 1.8,
    overlapPercent = cfg.overlapPercent / 5
  ))

  /*def combinedInput: CLA.Input = new IndexedSeq[Boolean] {
    def length: Bit = ThalamusActor.this.width

    def apply(idx: Bit): Boolean = sensoryInput(idx) || corticalInput(idx)
  }*/

  //TODO: just use 1 top state?
  def update(reward: Double): Unit = blocking {
    inputSDR.update(thalaSource)

    val overlapSeq = inputSDR.segments.map(x =>
      if(x.active) x.overlap else 0).toIndexedSeq

    update(overlapSeq, reward)
    publish(ThalamusOutput(produce))
    //if(math.random < 0.02) println(produce.map(x => if(x) 1 else 0).mkString)
  }

  def receive = neuralEntityActorReceive orElse {
    case ThalamusActor.ReceiveInput(input) =>
      require(input.length == thalamusInput.length)
      thalamusInput = input
    case ThalamusActor.Update(reward) => update(reward)
  }
}