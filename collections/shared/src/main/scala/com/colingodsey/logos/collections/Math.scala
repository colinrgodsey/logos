package com.colingodsey.logos.collections

object Math {

  final val TwoPi = math.Pi * 2

  def randomNormal: Double = randomNormal(1)

  //TODO: thread local spare (u2)?
  def randomNormal(variance: Double): Double = {
    def genU: (Double, Double, Double) = {
      val u1 = math.random * 2.0 - 1.0
      val u2 = math.random * 2.0 - 1.0

      val s = u1 * u1 + u2 * u2

      if(s >= 1 || s == 0) genU
      else (u1, u2, s)
    }

    val (u1, _, s) = genU

    val mul = math.sqrt(-2.0 * math.log(s) / s)

    //spare = u2 * mul
    variance * u1 * mul
  }
}
