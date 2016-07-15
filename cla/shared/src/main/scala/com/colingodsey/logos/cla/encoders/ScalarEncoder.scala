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
    min: Double = 0.0, max: Double = 1.0) { encoder =>
  val lengthMinusSize = length - size
  val range = max - min

  def source(x0: Double): CLA.InputSource = new CLA.InputSource {
    def width: Int = encoder.length

    def iterator: Iterator[Boolean] = produce.iterator
    override def produce = encode(x0)
  }

  def encode(x0: Double): CLA.Input = new IndexedSeq[Boolean] {
    val (areaMax, areaMin) = {
      val x = if(x0 > encoder.max) encoder.max
      else if(x0 < encoder.min) encoder.min
      else x0

      val value = (x - encoder.min) / range
      val a = (lengthMinusSize * value + encoder.size).toInt
      val b = (lengthMinusSize * value).toInt

      (a, b)
    }

    def length = encoder.length
    def apply(idx: Int) = idx < areaMax && idx >= areaMin

    override def toString = iterator.map(x => if(x) 1 else 0).mkString
  }
}

case class WeightedScalarEncoder(val length: Int, maxSize: Int,
    min: Double = 0.0, max: Double = 1.0) { encoder =>
  val range = max - min

  def source(x0: Double, scale: Double): CLA.InputSource = new CLA.InputSource {
    def width: Int = encoder.length

    def iterator: Iterator[Boolean] = produce.iterator
    override def produce = encode(x0, scale)
  }

  def encode(x0: Double, scale: Double): CLA.Input = new IndexedSeq[Boolean] {
    val (areaMax, areaMin) = {
      val x = if(x0 > encoder.max) encoder.max
      else if(x0 < encoder.min) encoder.min
      else x0

      val value = (x - encoder.min) / range

      val size = math.round(maxSize * scale).toInt
      val lengthMinusSize = length - size

      val a = (lengthMinusSize * value + size).toInt
      val b = (lengthMinusSize * value).toInt

      (a, b)
    }

    def length = encoder.length
    def apply(idx: Int) = idx < areaMax && idx >= areaMin

    override def toString = toSeq.map(x => if(x) 1 else 0).mkString
  }
}