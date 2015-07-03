package com.colingodsey.logos.collections

/**
 * Rolling average calculator
 * @param average - current average weighted by count
 * @param count - current units of sample
 * @param max - max units of sample
 */
final class RollingAverage(val max: Double,
    var average: Double = 0.0, var count: Double = 0.0) {

  def +=(n: Double): this.type = this += (n, 1.0)

  def +=(n: Double, weight: Double): this.type = {
    val newCount = math.min(max, count + weight)
    val newAverage = (average * (newCount - weight) + n) / newCount

    average = newAverage
    count = newCount

    this
  }

  @inline def toDouble = average
}

object RollingAverage {
  def apply(max: Int): RollingAverage = new RollingAverage(max)
  def apply(max: Int, first: Double): RollingAverage =
    new RollingAverage(max, average = first, count = 1)
}
/*
final case class RollingAverage(val max: Int,
    average: Double = 0, count: Int = 0) {

  def +(n: Double) = {
    val newCount = math.min(max, count + 1)
    val newAverage = (average * (newCount - 1) + n) / newCount

    copy(average = newAverage, count = newCount)
  }

  def toDouble = average
}

 */