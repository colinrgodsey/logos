package com.colingodsey.logos.collections

/**
 * Created by crgodsey on 6/2/15.
 */
case class RollingAverage(
    average: Double = 0, count: Int = 0)(val max: Int) {

  def +(n: Double) = {
    val newCount = math.min(max, count + 1)
    val newAverage = (average * (newCount - 1) + n) / newCount

    copy(average = newAverage, count = newCount)(max)
  }

  def toDouble = average
}
