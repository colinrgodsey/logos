package com.colingodsey.logos.qlearning

import com.colingodsey.logos.collections.{VecN, Vec}

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