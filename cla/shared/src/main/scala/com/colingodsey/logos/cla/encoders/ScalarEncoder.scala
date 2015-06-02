package com.colingodsey.logos.cla.encoders

import com.colingodsey.logos.cla.CLA

/**
 *
 * @param length - length in bits of the encoded value
 * @param size - width of the 'value block'- 5 size = ...000001111100000...
 * @param min - min value in the range
 * @param max - max value in the range
 */
case class ScalarEncoder(val length: Int, size: Int,
    min: Double = 0.0, max: Double = 1.0) {//extends CLA.Input {
  val lengthMinusSize = length - size

  def encode(x: Double): CLA.Input = new AnyRef {
    val (areaMax, areaMin) = {
      val y = x - min
      val value = math.max(0, y) / max
      val a = (lengthMinusSize * value + size).toInt
      val b = (lengthMinusSize * value).toInt

      (a, b)
    }

    def length = ScalarEncoder.this.length
    def apply(idx: Int) = idx < areaMax && idx >= areaMin

    def toSeq = for(i <- 0 until length) yield apply(i)
  }
}
