package com.colingodsey.logos.collections

import scala.util.Random

object Vec {
  def zero[T <: Vec](implicit vb: VecCompanion[T]) = vb.zero

  sealed trait Exception extends ArithmeticException

  case class NormalException(msg: String = "Cannot get the normal of a 0 vector") extends ArithmeticException(msg) with Vec.Exception
  case class NanException(msg: String = "NaN encountered") extends ArithmeticException(msg) with Vec.Exception
}

trait Vec extends VecOps[Vec] {
  /*def unary_-(): Vec
  def + (other: Vec): Vec
  def - (other: Vec): Vec
  def * (scale: Vec): Double

  def normal: Vec

  def toVecN: VecN*/

  def companion: VecCompanion[_]

  def toVecN: VecN

  def to[U <: Vec](implicit vb: VecCompanion[U]): U =
    if(vb != companion) vb(toVecN)
    else this.asInstanceOf[U]

  def values = toVecN.weights.values
  def keys = toVecN.weights.keys
}

trait VecOps[+Repr <: Vec] {
  def unary_-(): Vec
  def * (scale: Double): Vec
  def + (other: Vec): Vec
  def - (other: Vec): Vec
  def * (other: Vec): Double

  def normal: Vec
  def length: Double
}

trait VecBuilder[+To, -From]

object AnyVecBuilder extends VecBuilder[Vec, Vec] {

}

trait VecLike[Coll <: VecLike[Coll] with Vec] extends VecOps[Coll] with Vec {
  type This = Coll

  def unary_-(): Coll
  def * (scale: Double): Coll

  //need generics that convert
  /*def + [U <: Vec](that: U)(implicit builder: CanBuildFrom[This, Double, U]): U =
  this.to[U] + that

def to[U <: Vec](implicit builder: CanBuildFrom[This, Double, U]): U*/

  def + (other: Coll): Coll
  def - (other: Coll): Coll
  def * (other: Coll): Double

  def + (other: Vec): Coll = companion(other) + toVec
  def - (other: Vec): Coll = companion(other) - toVec
  def * (other: Vec): Double = companion(other) * toVec

  def add(other: Coll) = this + other
  def sub(other: Coll) = this - other
  def dot(other: Coll) = this * other
  def scale(s: Double) = this * s

  def toVec: Coll

  def companion: VecCompanion[This]

  def isNormal = length == 1
  def / (scale: Double): Coll = this * (1.0 / scale)

  def isOrigin: Boolean = (toVec * toVec) == 0

  //TODO: add isNormal, effecient normals

  def isAxisAligned: Boolean

  def lengthSq: Double = toVec * toVec

  def length: Double = {
    val r = math sqrt lengthSq
    require(!r.isNaN, toString + " made a NaN")
    r
  }

  def normal: Coll = {
    val l = length
    zeroNormRequire(l != 0)
    /*if(l == 0) companion.zero
    else */if(l == 1) toVec
    else toVec / l
  }

  def safeNormal: Coll = {
    val l = length
    if(l == 0) companion.zero
    else if(l == 1) toVec
    else normal
  }

  def length1: Double = values.foldLeft(0.0)(_ + math.abs(_))
  def lengthInf: Double = values.iterator.map(math.abs).max
  def isNaN = values.exists(_.isNaN)
  def isInfinite = values.exists(_.isInfinite)

  def clamp(l: Double): This = if(length > l) normal * l else toVec

  def ~~(other: This)(implicit epsilon: Epsilon): Boolean = {
    val sub = this - other

    sub.isOrigin || math.abs(sub.length) < epsilon.e
  }

  def !~~(other: This)(implicit epsilon: Epsilon): Boolean =
    !this.~~(other)

  def zeroNormRequire(cond: Boolean) = if(!cond) throw Vec.NormalException()
}

trait VecNumeric[T <: Vec] extends VecCompanion[T] with Numeric[T] {
  implicit def comp: VecNumeric[T] = this

  def plus(x: T, y: T): T = (x + y).to[T]
  def minus(x: T, y: T): T = (x - y).to[T]
  def times(x: T, y: T): T = Vec1(x * y).to[T] //TODO: this is sorta weird
  def negate(x: T): T = (-x).to[T]
  def fromInt(x: Int): T = (one * x).to[T]
  def toInt(x: T): Int = x.length.toInt
  def toLong(x: T): Long = x.length.toLong
  def toFloat(x: T): Float = x.length.toFloat
  def toDouble(x: T): Double = x.length

  def compare(x: T, y: T): Int = {
    val dl = x.length - y.length

    if(dl > 0) 1
    else if (dl < 0) -1
    else 0
  }

  //def zero = fromInt(0)
  //def one = fromInt(1)

  override def zero: T = origin
  override def one: T = unit
}

trait VecCompanion[+VecType <: Vec] {
  def origin: VecType
  def unit: VecType

  def apply(x: Vec): VecType

  //def dimensions: Dimensions

  def one: VecType// = unit
  def zero: VecType// = origin
}

case class Epsilon(e: Double)

object Epsilon {
  implicit val default = Epsilon(1e-12)

  def e(implicit ep: Epsilon) = ep.e
}

object Vec1 extends VecCompanion[Vec1] with VecNumeric[Vec1] {
  val origin = Vec1(0)
  val unit = Vec1(1)

  def apply(x: Vec): Vec1 = x match {
    case x: Vec1 => x
    case x => Vec1(x.toVecN("x"))
  }

  def dimensions: Dimensions = Dimensions.Two

  implicit def dTov1d(d: Double): Vec1 = Vec1(d)
  implicit def v1dToD(x: Vec1): Double = x.x
}

final case class Vec1(x: Double) extends Vec with VecLike[Vec1] {
  //only able to force this.type here because of final class
  def unary_-() = Vec1(-x).asInstanceOf[this.type]
  def + (other: Vec1) = Vec1(x + other.x).asInstanceOf[this.type]
  def - (other: Vec1) = Vec1(x - other.x).asInstanceOf[this.type]
  def * (scale: Double) = Vec1(x * scale).asInstanceOf[this.type]
  def * (other: Vec1): Double = x * other.x

  override def isOrigin = this == Vec1.origin
  def toVec = this

  def companion = Vec1

  lazy val isAxisAligned: Boolean = true

  override def length = x
  override lazy val normal: this.type = super.normal.asInstanceOf[this.type]

  def toVecN = MapVector("x" -> x)
}

object Vec2 extends VecCompanion[Vec2] with VecNumeric[Vec2] {
  val origin = Vec2(0, 0)
  val unit = Vec2(1, 1)

  def dimensions: Dimensions = Dimensions.Two

  def randomNonNormal = Vec2(math.random * 2 - 1, math.random * 2 - 1)
  def random = randomNonNormal.normal

  def apply(x: Vec): Vec2 = x match {
    case x: Vec2 => x
    case x =>
      val vn = x.toVecN
      Vec2(vn("x"), vn("y"))
  }
}

final case class Vec2(x: Double, y: Double) extends Vec with VecLike[Vec2] {
  final def unary_-() = Vec2(-x, -y)
  def + (other: Vec2) = Vec2(x + other.x, y + other.y).asInstanceOf[this.type]
  def - (other: Vec2) = Vec2(x - other.x, y - other.y).asInstanceOf[this.type]
  def * (scale: Double) = Vec2(x * scale, y * scale).asInstanceOf[this.type]
  def * (other: Vec2): Double = x * other.x + y * other.y

  override def isOrigin = this == Vec2.origin
  def toVec = this

  def companion = Vec2

  lazy val isAxisAligned: Boolean = Seq(x, y).count(_ != 0) == 1

  override lazy val length = super.length
  override lazy val normal = super.normal

  def toVecN = MapVector("x" -> x, "y" -> y)
}

object Vec3 extends VecCompanion[Vec3] with VecNumeric[Vec3] {
  val origin = Vec3(0, 0, 0)
  val unit = Vec3(1, 1, 1)

  val x = Vec3(1, 0, 0)
  val y = Vec3(0, 1, 0)
  val z = Vec3(0, 0, 1)

  val mx = -x
  val my = -y
  val mz = -z

  def random: Vec3 = (Vec3(math.random, math.random,
    math.random) * 2 - Vec3.one).normal

  def random(seed: Int): Vec3 = {
    val r = new Random(seed)

    (Vec3(r.nextDouble(), r.nextDouble(),
      r.nextDouble()) * 2 - Vec3.one).normal
  }

  def dimensions: Dimensions = Dimensions.Two

  //azimuthal
  def angle(alpha: Double, delta: Double) = {
    val cosDelta = math.cos(delta)
    val x = math.cos(alpha) * cosDelta
    val y = math.sin(alpha) * cosDelta
    val z = math.sin(delta)

    Vec3(x, y, z)
  }

  def apply(x: Vec): Vec3 = x match {
    case x: Vec3 => x
    case x =>
      val vn = x.toVecN
      Vec3(vn("x"), vn("y"), vn("z"))
  }
}

final case class Vec3(x: Double, y: Double, z: Double) extends Vec with VecLike[Vec3] {
  def unary_-() = Vec3(-x, -y, -z).asInstanceOf[this.type]
  def + (other: Vec3) = Vec3(x + other.x, y + other.y, z + other.z).asInstanceOf[this.type]
  def - (other: Vec3) = Vec3(x - other.x, y - other.y, z - other.z).asInstanceOf[this.type]
  def * (scale: Double) = Vec3(x * scale, y * scale, z * scale).asInstanceOf[this.type]
  def * (other: Vec3): Double = x * other.x + y * other.y + z * other.z

  if(isNaN) throw Vec.NanException("NaN encountered upn creation")

  override def isOrigin = this == Vec3.origin

  def toVec = this
  def companion = Vec3

  lazy val isAxisAligned: Boolean = Seq(x, y, z).count(_ != 0) == 1

  override lazy val length = super.length
  override lazy val normal = super.normal

  def toVecN = MapVector("x" -> x, "y" -> y, "z" -> z)
  def toDoubleArray: Seq[Double] = Seq(x, y, z)

  def alpha = math.atan2(y, x)
  def delta = math.asin(z / length)

  def x (other: Vec3): Vec3 = Vec3(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x
  )

  def cross(other: Vec3): Vec3 = this x other

  def angle(other: Vec3) = {
    val np = length * other.length

    zeroNormRequire(np != 0)

    val dot = this * other
    val thresh = np * 0.9999

    if(dot < -thresh || dot > thresh) {
      val cross = this x other
      val asin = Math.asin(cross.length / np)
      if(dot >= 0) asin
      else Math.PI - asin
    } else Math.acos(dot / np)
  }

  def orthogonal = {
    val thresh = 0.6 * length

    zeroNormRequire(thresh != 0)

    if(math.abs(x) <= thresh) {
      val i = 1.0 / math.sqrt(y * y + z * z)
      Vec3(0, i * z, -i * y)
    } else if(math.abs(y) <= thresh) {
      val i = 1.0 / math.sqrt(x * x + z * z)
      Vec3(-i * z, 0, i * x)
    } else {
      val i = 1.0 / math.sqrt(x * x + y * y)
      Vec3(i * y, -i * x, 0)
    }
  }

  def round = Vec3(x.toInt, y.toInt, z.toInt)

  //def apply(other: Point3D) = Point3D(x * other.x, y * other.y, z * other.z)
}

object Vec4 extends VecCompanion[Vec4] with VecNumeric[Vec4] {
  val origin = Vec4(0, 0, 0, 0)
  val unit = Vec4(1, 1, 1, 1)

  def random = (Vec4(math.random, math.random,
    math.random, math.random) * 2 - Vec4.one).normal

  def dimensions: Dimensions = Dimensions.Two

  def apply(x: Vec): Vec4 = x match {
    case x: Vec4 => x
    case x =>
      val vn = x.toVecN
      Vec4(vn("x"), vn("y"), vn("z"), vn("a"))
  }
}

final case class Vec4(x: Double, y: Double, z: Double, a: Double) extends Vec with VecLike[Vec4] {
  def unary_-() = Vec4(-x, -y, -z, -a).asInstanceOf[this.type]
  def + (other: Vec4) = Vec4(x + other.x, y + other.y, z + other.z, a + other.a).asInstanceOf[this.type]
  def - (other: Vec4) = Vec4(x - other.x, y - other.y, z - other.z, a - other.a).asInstanceOf[this.type]
  def * (scale: Double) = Vec4(x * scale, y * scale, z * scale, a * scale).asInstanceOf[this.type]
  def * (other: Vec4): Double = x * other.x + y * other.y + z * other.z + a * other.a

  if(isNaN) throw Vec.NanException("NaN encountered upn creation")

  override def isOrigin = this == Vec4.origin

  def toVec = this
  def companion = Vec4

  lazy val isAxisAligned: Boolean = Seq(x, y, z, a).count(_ != 0) == 1

  override lazy val length = super.length
  override lazy val normal = super.normal

  def toVecN = MapVector("x" -> x, "y" -> y, "z" -> z, "a" -> a)
  def toDoubleArray: Seq[Double] = Seq(x, y, z, a)

  def round = Vec4(x.toInt, y.toInt, z.toInt, a.toInt)

  //def apply(other: Point3D) = Point3D(x * other.x, y * other.y, z * other.z)
}