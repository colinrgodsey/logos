package com.colingodsey.logos.collections

import scala.annotation.tailrec
import scala.util.Random

object ExtraMath {
  final val TwoPi = math.Pi * 2
  final val SqrtTwoPi = math.sqrt(TwoPi)
  final val SqrtTwo = math.sqrt(2.0)
  final val FirstSDev = 0.682689492137086
  final val MajorTolerance = 1e-05

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

  def normalPDF(x: Double, µ: Double = 0.0, σ: Double = 1.0): Double = {
    val ePower = -math.pow(x - µ, 2) / (2 * σ * σ)

    math.pow(math.E, ePower) / (σ * SqrtTwoPi)
  }

  //1st integral of normal pdf
  def normalCDF(x: Double, µ: Double, σ: Double): Double = {
    val erfX = (x - µ) / (σ * SqrtTwo)

    //constant not needed?
    math.abs(erf(erfX)) * 0.5/* + 0.5*/
  }

  //cdf for a range x1 - x2
  def normalCDF(x: Double, dx: Double, µ: Double, σ: Double): Double = {
    val r1 = normalCDF(x, µ, σ)
    val r2 = normalCDF(x + dx, µ, σ)

    math.abs(r1 - r2)
  }

  //http://picomath.org/javascript/erf.js.html
  def erf(x0: Double): Double = { //best
    val a1 =  0.254829592
    val a2 = -0.284496736
    val a3 =  1.421413741
    val a4 = -1.453152027
    val a5 =  1.061405429
    val p  =  0.3275911

    val sign = if (x0 < 0) -1.0 else 1.0

    val x = math.abs(x0)

    // A&S formula 7.1.26
    val t = 1.0 / (1.0 + p * x)
    val y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * math.exp(-x * x)

    sign * y
  }

  //http://introcs.cs.princeton.edu/java/21function/ErrorFunction.java.html
  def erf2(z: Double): Double = { //ok, does well at testErfGnu tho
    val t: Double = 1.0 / (1.0 + 0.5 * math.abs(z))
    val ans: Double = 1 - t * math.exp(-z * z - 1.26551223 + t *
        (1.00002368 + t *
            (0.37409196 + t *
                (0.09678418 + t *
                    (-0.18628806 + t *
                        (0.27886807 + t *
                            (-1.13520398 + t *
                                (1.48851587 + t *
                                    (-0.82215223 + t * 0.17087277) ))))))))

    if (z >= 0) ans
    else -ans
  }

  //http://introcs.cs.princeton.edu/java/21function/ErrorFunction.java.html
  def erf3(z: Double): Double = { //worst
    val t: Double = 1.0 / (1.0 + 0.47047 * math.abs(z))
    val poly: Double = t * (0.3480242 + t * (-0.0958798 + t * 0.7478556))
    val ans: Double = 1.0 - poly * math.exp(-z * z)

    if (z >= 0) ans
    else -ans
  }

}
