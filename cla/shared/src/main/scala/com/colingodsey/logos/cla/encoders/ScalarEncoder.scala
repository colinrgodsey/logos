package com.colingodsey.logos.cla.encoders

import com.colingodsey.logos.cla.CLA

/**
 * Created by crgodsey on 6/2/15.
 */
case class ScalarEncoder(val length: Int, size: Int, max: Double = 1.0) {//extends CLA.Input {
  val lengthMinusSize = length - size

  def encode(x: Double): CLA.Input = new AnyRef {
    val value = math.max(0, x) / max
    val areaMax = (lengthMinusSize * value + size).toInt
    val areaMin = (lengthMinusSize * value).toInt

    def length = ScalarEncoder.this.length
    def apply(idx: Int) = idx < areaMax && idx >= areaMin

    def toSeq = for(i <- 0 until length) yield apply(i)
  }
}
