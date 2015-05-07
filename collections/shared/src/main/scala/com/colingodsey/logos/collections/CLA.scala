package com.colingodsey.logos.collections

/**
 * Cortical Learning Algorithm inspired learner
 *
 * All vectors must be in the positive domain
 *
 * TODO: dimension in feature vector that determins internal/external impulse?
 *    whether we want to visualize the result, or act on it
 *
 * TODO: should we filter out impossible parts of the impulse before recursing?
 *    or do we rely on sensory input to feed back if performed
 *
 * TODO: order of node processing for nodes?
 *
 * TODO: connection weight. strength as a measure of how well its learned
 *
 * TODO: should output just be the feature vector of all active nodes?
 *
 * # Output of an impulse should be the same as the sum of the output of
 * # the individual output components.
 * output(impulse) = x
 * x == output(x1, x2, ...)
 *
 * Assume a trained network on cars.
 * The output of a car impulse may be composed of the output of impulses
 * for the windows, seat, wheels. The definition of a car could be assumed
 * to be the collection of just those 3 impulses.
 *
 * output(house) == output(window, seat)
 * output(car) == output(window, seat, wheel)
 * output(house, wheel) == output(car)
 *
 * Each of the component impulses may build a higher dimension in the feature
 * vector for "car".
 *
 * For classification, sort by highest dimension of normalized output vector.
 *
 * Reverse activation- high-order dimensions adds feature vector instead of identity vector?
 *     going from car -> window -> glass, etc. Weighted by dot product of feature * id.
 *     This is slower, but its a more recursive recall.
 *     Is this necessary? Will input impulse ever not contain subcomponents?
 *     How much do connections matter?
 *     Connections probably dont have to be a tree... ?
 *
 * Learn:
 *    error based on vector direction, not magnitude (normals)
 */
object CLA {
  type Impulse = VecN

  val NoImpulse = VecN.zero

  /*
  val dot = impulse * feature
  if(dat > activation) impulse += VecN(id -> 1)

  or....

  val dot = impulse(id)
  if(dat > activation) impulse += feature



  feature = weighted combination of subfeatures features
   */
  case class Node(id: String, feature: VecN = VecN.zero) {
    val fNormal = feature.safeNormal
    def activationFeature = VecN(id -> 1.0)
  }
}
class CLA {
  import CLA._

  def outputFeedbackScalar = 0.3 //internal cognition
  def activationThreshold = 0.2
  def currentImpulseAlpha = 0.1 //internal recursion
  def errorRatio = 0.01

  var currentImpulse = NoImpulse
  var currentOutput = NoImpulse
  //var headNode = Node("head", NoImpulse, Set.empty)
  var nodes = Set.empty[Node]

  //TODO: on connection use (if activates connecting), strengthen
  /*private def calculateOutput(node: Node = headNode, impulse: Impulse = currentImpulse): Impulse = {
    if(node.feature * impulse > activationThreshold) {
      val activatedImpulse = impulse + node.activationFeature
      val childValues = node.connections.map(calculateOutput(_, activatedImpulse)).sum

      activatedImpulse + childValues
    } else NoImpulse
  }*/
  private def calculateOutput(impulse: Impulse): Impulse = {
    //TODO: order is probably super important here
    val sumActivation = nodes.map { node =>
      val dot = node.fNormal * impulse

      if (dot > activationThreshold) node.activationFeature
      else NoImpulse
    }.sum

    impulse * currentImpulseAlpha + sumActivation * (1.0 - currentImpulseAlpha)
  }

  //maybe think when sensory input gets too low (below a certain threshold)
  def think(): Unit = advance(currentImpulse)

  //add random weights
  def shake(c: Double): Unit = {
    val keys = nodes.map(_.id).toSet

    nodes = nodes map { node =>
      val dims = keys.map(x => x -> math.random).toSeq
      val v = VecN(dims: _*).normal * c

      node.copy(feature = node.feature + v)
    }
  }

  def learn(error: Impulse): Unit = {
    nodes = nodes map { node =>
      val eD = currentImpulse.safeNormal * node.fNormal
      val newFeature = if(eD > 0) {
        error * (eD * errorRatio) + node.feature
      } else node.feature

      //val newFeature = error * (eD * errorRatio) + node.feature

      node.copy(feature = newFeature)
    }
  }

  /*
  Act on external sensory impulse. Recursive impulses handled internally,
  as those are left out of the error gradient.

  Return output impulse
   */
  def advance(impulse0: Impulse = NoImpulse): Impulse = {
    val impulse = impulse0.safeNormal

    //error vector
    //val error = impulse - currentOutput
    val error = impulse - currentImpulse

    //errr should really affect boolean states.... not floating point
    learn(error)

    //TODO: currentImpulse for recursion?
    //TODO: normal or not?
    //currentImpulse = impulse * (1.0 - outputAlpha) + currentOutput * outputAlpha
    currentImpulse = (impulse + currentOutput * outputFeedbackScalar).safeNormal
    currentOutput = calculateOutput(currentImpulse).safeNormal

    currentOutput
  }
}
