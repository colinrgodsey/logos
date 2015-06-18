package com.colingodsey.logos.collections

import scala.annotation.tailrec
import scala.util.Random

object ExtraMath {
  final val TwoPi = math.Pi * 2

  def randomNormal: Double = randomNormal(1)

  //TODO: thread local spare (u2)?

  def randomNormal(variance: Double, r: Random = Random): Double = {
    val (u1, _, s) = genU(r)

    val mul = math.sqrt(-2.0 * math.log(s) / s)

    //spare = u2 * mul
    variance * u1 * mul
  }

  @tailrec
  private def genU(r: Random): (Double, Double, Double) = {
    val u1 = r.nextDouble() * 2.0 - 1.0
    val u2 = r.nextDouble() * 2.0 - 1.0

    val s = u1 * u1 + u2 * u2

    if(s >= 1 || s == 0) genU(r)
    else (u1, u2, s)
  }
}
