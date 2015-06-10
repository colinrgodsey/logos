package com.colingodsey.logos.collections

/**
 * Rolling average calculator
 * @param average - current average weighted by count
 * @param count - current count of samples
 * @param max - max number of samples
 */
final class RollingAverage(val max: Int,
    var average: Double = 0, var count: Int = 0) {

  def +=(n: Double) = {
    val newCount = math.min(max, count + 1)
    val newAverage = (average * (newCount - 1) + n) / newCount

    average = newAverage
    count = newCount
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