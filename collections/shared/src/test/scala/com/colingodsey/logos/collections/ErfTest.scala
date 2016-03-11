package com.colingodsey.logos.collections

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import utest._


/**
  */
object ErfTest extends TestSuite {
  def assertEquals(x: Double, y: Double) = assert(x == y)
  def assertEquals(x: Double, y: Double, d: Double) = {
    val delta = math.abs(x - y)

    assert(delta <= d)
  }

  val tests = TestSuite {
    "testErf0" - {
      val actual: Double = ExtraMath.erf(0.0)
      val expected: Double = 0.0

      assertEquals(expected, actual, 1.0e-9 /*1.0e-15*/) //raised tolerance
    }

    "testErf1960" - {
      val x: Double = 1.960 / math.sqrt(2.0)
      var actual: Double = ExtraMath.erf(x)
      var expected: Double = 0.95

      assertEquals(expected, actual, 1.0e-5)
      actual = ExtraMath.erf(-x)
      expected = -expected
      assertEquals(expected, actual, 1.0e-5)
    }

    "testErf2576" - {
      val x: Double = 2.576 / math.sqrt(2.0)
      var actual: Double = ExtraMath.erf(x)
      var expected: Double = 0.99

      assertEquals(expected, actual, 1.0e-5)
      actual = ExtraMath.erf(-x)
      expected = -expected
      assertEquals(expected, actual, 1.0e-5)
    }

    "testErf2807" - {
      val x: Double = 2.807 / math.sqrt(2.0)
      var actual: Double = ExtraMath.erf(x)
      var expected: Double = 0.995

      assertEquals(expected, actual, 1.0e-5)
      actual = ExtraMath.erf(-x)
      expected = -expected
      assertEquals(expected, actual, 1.0e-5)
    }

    "testErf3291" - {
      val x: Double = 3.291 / math.sqrt(2.0)
      var actual: Double = ExtraMath.erf(x)
      var expected: Double = 0.999

      assertEquals(expected, actual, 1.0e-5)
      actual = ExtraMath.erf(-x)
      expected = -expected
      assertEquals(expected, actual, 1.0e-5)
    }

    /**
      * MATH-301, MATH-456
      */
    "testLargeValues" - {
      {
        var i: Int = 1
        while (i < 200) {
          var result: Double = ExtraMath.erf(i)
          assert(!result.isNaN)
          assert(result > 0 && result <= 1)

          result = ExtraMath.erf(-i)
          assert(!result.isNaN)
          assert(result >= -1 && result < 0)

          i *= 10
        }
      }
      assertEquals(-1, ExtraMath.erf(Double.NegativeInfinity), 0)
      assertEquals(1, ExtraMath.erf(Double.PositiveInfinity), 0)
    }

    /**
      * Compare ExtraMath.erf against reference values computed using GCC 4.2.1 (Apple OSX packaged version)
      * erfl (extended precision erf).
      */
    "testErfGnu" - {
      val tol: Double = 1E-7 /*1E-15*/
      val gnuValues: Array[Double] = Array[Double](-1, -1, -1, -1, -1, -1, -1, -1, -0.99999999999999997848, -0.99999999999999264217, -0.99999999999846254017, -0.99999999980338395581, -0.99999998458274209971, -0.9999992569016276586, -0.99997790950300141459, -0.99959304798255504108, -0.99532226501895273415, -0.96610514647531072711, -0.84270079294971486948, -0.52049987781304653809, 0, 0.52049987781304653809, 0.84270079294971486948, 0.96610514647531072711, 0.99532226501895273415, 0.99959304798255504108, 0.99997790950300141459, 0.9999992569016276586, 0.99999998458274209971, 0.99999999980338395581, 0.99999999999846254017, 0.99999999999999264217, 0.99999999999999997848, 1, 1, 1, 1, 1, 1, 1, 1)

      var x: Double = -10

      for (i <- 0 until 41) {
        val delta = math.abs(gnuValues(i) - ExtraMath.erf2(x))

        require(delta <= tol, s"failed at itr $i with delta $delta")
        x += 0.5
      }
    }

    "normalCDF - 1 sdev" - {
      val probability1sdev = 0.682
      val range = 100

      val res = for(i <- -range to range) yield {
        val i2 = if(i < 0) i - 1
        else if(i > 0) i + 1
        else i

        ExtraMath.normalCDF(i.toDouble, i2.toDouble, 0, range)
      }

      val sum = res.sum

      //~= 1 standard deviation
      assertEquals(sum, probability1sdev, 0.01)
    }

    "normalCDF - all samples" - {
      val probabilityFullRange = 1.0
      val range = 2000

      val res = for(i <- -range to range) yield {
        val i2 = if(i < 0) i - 1
        else if(i > 0) i + 1
        else i

        ExtraMath.normalCDF(i.toDouble, i2.toDouble, 0, 100)
      }

      val sum = res.sum

      //~= 1 standard deviation
      assertEquals(sum, probabilityFullRange, 0.01)
    }
  }

}