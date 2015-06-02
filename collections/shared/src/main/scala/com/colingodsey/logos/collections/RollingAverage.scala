package com.colingodsey.logos.collections

/**
 * Rolling average calculator
 * @param average - current average weighted by count
 * @param count - current count of samples
 * @param max - max number of samples
 */
case class RollingAverage(val max: Int,
    average: Double = 0, count: Int = 0) {

  def +(n: Double) = {
    val newCount = math.min(max, count + 1)
    val newAverage = (average * (newCount - 1) + n) / newCount

    copy(average = newAverage, count = newCount)
  }

  def toDouble = average
}
