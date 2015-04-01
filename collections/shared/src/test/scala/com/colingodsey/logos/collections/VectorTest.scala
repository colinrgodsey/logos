package com.colingodsey.logos.collections

import scala.util.control.NonFatal

import utest._

//taken from apache commonds Vector3D
object VectorTest extends TestSuite {
  implicit def ep = Epsilon.default

  def checkVector(v1: Vec3, x: Double, y: Double, z: Double): Unit = {
    checkVector(v1, Vec3(x, y, z))
  }

  def checkVectorEp(v1: Vec3, x: Double, y: Double, z: Double): Unit = {
    checkVectorEp(v1, Vec3(x, y, z))
  }

  def checkVector(v1: Vec3, v2: Vec3): Unit = {
    assertEquals(v1, v2)
  }

  def checkVectorEp(v1: Vec3, v2: Vec3): Unit = {
    require(v1 ~~ v2, s"$v1 !~~ $v2 (e = ${Epsilon.e}})")
  }

  def assertEquals(a: Double, b: Double, e: Double): Unit =
    require(math.abs(a - b) <= e, s"abs($a - $b) > $e")

  def assertEquals(a: Any, b: Any): Unit =
    require(a == b, s"$a != $b")

  def assertTrue(b: Boolean) = require(b)

  val tests = TestSuite {

    "testScaling" - {
      val r = math.sqrt(2) / 2
      checkVectorEp(Vec3.angle(math.Pi / 3, -math.Pi / 4) * 2,
        r, r * math.sqrt(3), -2 * r)
      checkVector(Vec3.x * 2 + Vec3.mz * -3,
        2, 0, 3)
      checkVector(
        Vec3.x * 2 + Vec3.y * 5 + Vec3.mz * -3,
        2, 5, 3)
      checkVector(Vec3.x * 2 + Vec3.y * 5 + Vec3.my * 5 + Vec3.mz * -3,
        2, 0, 3)
    }

    "testCoordinates" - {
      val v = Vec3(1, 2, 3)
      assertTrue(math.abs(v.x - 1) < Epsilon.e)
      assertTrue(math.abs(v.y - 2) < Epsilon.e)
      assertTrue(math.abs(v.z - 3) < Epsilon.e)
    }

    "testNorm1" - {
      assertEquals(0.0, Vec3.zero.length1)
      assertEquals(6.0, Vec3(1, -2, 3).length1, 0)
    }

    "testNorm" - {
      assertEquals(0.0, Vec3.zero.length)
      assertEquals(math.sqrt(14), Vec3(1, 2, 3).length, Epsilon.e)
    }

    "testNormInf" - {
      assertEquals(0.0, Vec3.zero.lengthInf)
      assertEquals(3.0, Vec3(1, -2, 3).lengthInf, 0)
    }

    "testDistance1" - {
      val v1 = Vec3(1, -2, 3)
      val v2 = Vec3(-4, 2, 0)
      assertEquals(0.0, (Vec3.mx - Vec3.mx).length1, 0)
      assertEquals(12.0, (v1 - v2).length1, Epsilon.e)
      assertEquals((v1 - v2).length1, (v2 - v1).length1, Epsilon.e)
    }

    "testDistance" - {
      val v1 = Vec3(1, -2, 3)
      val v2 = Vec3(-4, 2, 0)
      assertEquals(0.0, (Vec3.mx - Vec3.mx).length, 0)
      assertEquals(Math.sqrt(50), (v1 - v2).length, Epsilon.e)
      assertEquals((v1 - v2).length, (v2 - v1).length, Epsilon.e)
    }

    "testDistanceSq" - {
      val v1 = Vec3(1, -2, 3)
      val v2 = Vec3(-4, 2, 0)

      assertEquals(0.0, (Vec3.mx - Vec3.mx).lengthSq, 0)
      assertEquals(50.0, (v1 - v2).lengthSq, Epsilon.e)
      assertEquals((v2 - v1).length * (v2 - v1).length,
        (v2 - v1).lengthSq, Epsilon.e)
    }

    "testDistanceInf" - {
      val v1 = Vec3(1, -2, 3)
      val v2 = Vec3(-4, 2, 0)

      assertEquals(0.0, (Vec3.mx - Vec3.mx).lengthInf, 0)
      assertEquals(5.0, (v1 - v2).lengthInf, Epsilon.e)
    }

    "testSubtract" - {
      val v2 = Vec3(-3, -2, -1)
      val v1 = Vec3(1, 2, 3) - v2

      checkVector(v1, 4, 4, 4)
      checkVector(v2 - v1, -7, -6, -5)
      checkVector(v2 - v1 * 3, -15, -14, -13)

    }

    "testAdd" - {
      val v2 = Vec3(-3, -2, -1)
      val v1 = Vec3(1, 2, 3) + v2

      checkVector(v1, -2, 0, 2)
      checkVector(v2 + v1, -5, -2, 1)
      checkVector(v2 + v1 * 3, -9, -2, 5)
    }

    "testScalarProduct" - {
      val v = Vec3(1, 2, 3) * 3

      checkVector(v, 3, 6, 9)
      checkVector(v * 0.5, 1.5, 3, 4.5)
      checkVector(v / 2, 1.5, 3, 4.5)

    }

    "testVectorialProducts" - {
      val v1 = Vec3(2, 1, -4)
      val v2 = Vec3(3, 1, -1)

      assertTrue(math.abs(v1 * v2 - 11) < Epsilon.e)

      val v3 = v1 x v2
      checkVector(v3, 3, -10, -1)

      assertTrue(math.abs(v1 * v3) < Epsilon.e)
      assertTrue(math.abs(v2 * v3) < Epsilon.e)

    }

    "testAngular" - {
      assertEquals(0, Vec3.x.alpha, 1.0e-10)
      assertEquals(0, Vec3.x.delta, 1.0e-10)
      assertEquals(math.Pi / 2, Vec3.y.alpha, 1.0e-10)
      assertEquals(0, Vec3.y.delta, 1.0e-10)
      assertEquals(0, Vec3.z.alpha, 1.0e-10)
      assertEquals(math.Pi / 2, Vec3.z.delta, 1.0e-10)

      val u = Vec3(-1, 1, -1)
      assertEquals(3 * math.Pi / 4, u.alpha, 1.0e-10)
      assertEquals(-1.0 / math.sqrt(3), math.sin(u.delta), 1.0e-10)

    }

    "testAngularSeparation" - {
      val v1 = Vec3(2, -1, 4)

      val k = v1.normal
      val i = k.orthogonal
      val v2 = k * math.cos(1.2) + i * math.sin(1.2)

      assertTrue(math.abs(v1.angle(v2) - 1.2) < Epsilon.e)

    }

    "testnormal" - {
      assertEquals(1.0, Vec3(5, -4, 2).normal.length, Epsilon.e)
      try {
        Vec3.zero.normal
        sys.error("an exception should have been thrown")
      } catch {
        case _: ArithmeticException =>
        case NonFatal(x) =>
          x.printStackTrace()
          sys.error("wrong exception caught: " + x.getMessage())
      }
    }

    "testOrthogonal" - {
      val v1 = Vec3(0.1, 2.5, 1.3)
      assertEquals(0.0, v1 * v1.orthogonal, Epsilon.e)
      val v2 = Vec3(2.3, -0.003, 7.6)
      assertEquals(0.0, v2 * v2.orthogonal, Epsilon.e)
      val v3 = Vec3(-1.7, 1.4, 0.2)
      assertEquals(0.0, v3 * v3.orthogonal, Epsilon.e)

      try {
        Vec3.zero.orthogonal
        sys.error("an exception should have been thrown")
      } catch {
        case _: ArithmeticException =>
        case NonFatal(x) =>
          x.printStackTrace()
          sys.error("wrong exception caught: " + x.getMessage())
      }
    }

    "testAngle" - {
      assertEquals(0.22572612855273393616,
        Vec3(1, 2, 3) angle Vec3(4, 5, 6),
        Epsilon.e)
      assertEquals(7.98595620686106654517199e-8,
        Vec3(1, 2, 3) angle Vec3(2, 4, 6.000001),
        Epsilon.e)
      assertEquals(3.14159257373023116985197793156,
        Vec3(1, 2, 3) angle Vec3(-2, -4, -6.000001),
        Epsilon.e)
      try {
        Vec3.zero angle Vec3.x
        sys.error("an exception should have been thrown")
      } catch {
        case _: ArithmeticException =>
        case NonFatal(x) =>
          x.printStackTrace()
          sys.error("wrong exception caught: " + x.getMessage())
      }
    }
  }

}