package com.colingodsey.logos.qlearning

import com.colingodsey.logos.collections.{VecN, Vec}

import scala.annotation.tailrec

trait Selector {
  def selectFrom[T](policy: TraversableOnce[(T, Double)]): T
  def streamFrom[T](policy: TraversableOnce[(T, Double)]): Iterator[T]

  def selectFrom[T](policy: Map[T, Double]): T =
    selectFrom(policy.iterator)

  def selectFrom[T](vals: Set[T])(f: T => Double): T =
    selectFrom(vals.map(x => x -> f(x)).toMap)
}


trait QLearningValues {
  //gamma, how much the max q of associated state is blended in. higer means more foresight
  def γ: Double

  //alpha, familiarity, learning factor. higher means slower
  def α0: Double

  def gamma = γ
  def alpha0 = α0
}

/**
 * Base QLearning mechanism
 * @tparam T - Numeric-like q-value type
 */
trait QLearning[T] extends QLearningValues {
  def selector: Selector
  def desire: T
  def plus(a: T, b: T): T
  def dot(a: T, b: T): Double
  def scale(a: T, b: Double): T


  //old + rate(r + gamma * max - old)

  //max = estimate of optimal future value
  def update(oldQ: T, maxNextQ: T, reward: T): T = {
    //reward + oldQ * α0 + maxNextQ * (1.0 - α0)
    //oldQ + α0 * (r + γ * max - oldQ)
    plus(
      plus(reward, scale(oldQ, α0)),
      scale(scale(maxNextQ, γ), 1.0 - α0))

    ???
  }

  def policy[U](actions: Map[U, T]): U = {
    val weights = actions.map {
      case (key, value) => key -> dot(value, desire)
    }

    selector.selectFrom(actions.keySet) { action =>
      dot(actions(action), desire)
    }
  }
}

trait DoubleQLearning extends QLearning[Double] {
  def desire = 1.0 //with scalar desires, generally just want 1

  def plus(a: Double, b: Double): Double = a + b
  def dot(a: Double, b: Double): Double = a * b
  def scale(a: Double, b: Double): Double = a * b
}


trait VecQLearning extends QLearning[Vec] {
  def plus(a: Vec, b: Vec): Vec = a + b
  def dot(a: Vec, b: Vec): Double = a * b
  def scale(a: Vec, b: Double): Vec = a * b
}

trait VecNQLearning extends QLearning[VecN] {
  def plus(a: VecN, b: VecN): VecN = a + b
  def dot(a: VecN, b: VecN): Double = a * b
  def scale(a: VecN, b: Double): VecN = a * b
}