package com.colingodsey.logos.cla

import com.colingodsey.logos.cla.CLA.Input
import com.colingodsey.logos.collections.VecN
import com.colingodsey.logos.qlearning._

import scala.collection.BitSet
import scala.collection.mutable

//maybe HTM like, burst columns when rewarding/acting, predict when not.

//q-learning like hypothesis- relate reward to the SDR (#1) that created it.
//when acting in a way (#2) that leads you to an SDR #1, share some of that reward.
//basically, reward states that lead to states (etc) that lead to a reward

//could be done with HTM- only make distal prediction when under a reward, plus get a reward for
//correctly predicting. unfortunately changes spatial properties

//TODO: thalamus should maybe affect synapse weight more if current Q is high or low. specializes in just learning reward conditions

//TODO: maybe randomly weight possible 'actions' with an input like we do in -> out bits in inputSDR

//TODO: use QLearning with a state vector per column. use combined vector for state transitions.
class SimpleThalamus(val width: Int, val minDesiredActivity: Int) extends Thalamus {
  //get weighted list of all possible actions, plus a 'default' weighting for a random 'new' action

  //an action that can also be taken is to 'cognate', or just recurse some input

  //TODO: boltzmann selector with temperature based on 'panic' ?
  val selector: Selector = BoltzmannSelector()
  val allActions: Set[Int] = (0 until width).toSet

  private val qValues = new Array[Double](width * width)
  protected var currentInput: IndexedSeq[Double] = new Array[Double](width)
  protected var selectedAction: CLA.Input = new Array[Boolean](width)

  def α0: Double = 0.3 //ratio of old q, higher is faster

  //gamma, how much the max q of associated state is blended in
  def γ: Double = 0.1

  @inline private def idxFor(s: State, a: Action): Int = s * width + a

  def getQ(s: State, a: Action): Weight =
    qValues(idxFor(s, a))

  def setQ(s: State, a: Action, q: Weight): Unit =
    qValues(idxFor(s, a)) = q

  def weightsIterator(s: State): Iterator[Weight] = {
    Iterator.from(0).take(width).map(getQ(s, _))
  }

  def generateOutput(): Input = {
    val actions = selectActionWeighted(currentInput)
    val out = new Array[Boolean](width)

    actions.foreach(action => out(action) = true)

    out
  }

  def maxQFor(input: IndexedSeq[Double]): Weight = {
    val maxOverlap = input.max

    val itr = for {
      state: State <- (0 until width).iterator
      overlap = input(state)
      if overlap != 0
      w <- weightsIterator(state)
    } yield w * overlap

    if(itr.hasNext) itr.max / maxOverlap
    else 0.0
  }

  def update(state: State, action: Action, maxNextQ: Weight, reward: Double, overlapRatio: Double): Unit = {
    val oldQ = getQ(state, action)
    //val newQ = math.max(update(oldQ, maxNextQ, reward), 0)

    val α = α0 * overlapRatio

    val newQ = oldQ + α * (reward + γ * maxNextQ - oldQ)

    //if(math.random < 0.002) println("newq " + newQ, state -> action, reward)

    setQ(state, action, newQ)
  }

  def update(newDoubleInput: IndexedSeq[Double], reward: Double): Unit = {
    val newInput = newDoubleInput.map(_ != 0)
    require(newInput.length == width)

    val oldMaxOverlap = currentInput.max + 1

    val maxOverlap = newDoubleInput.max + 1
    val maxQ = maxQFor(newDoubleInput) / maxOverlap

    for {
      state: State <- (0 until width).iterator
      if currentInput(state) != 0.0
      overlapRatio = currentInput(state) / oldMaxOverlap
      action: Action <- 0 until width
      if selectedAction(action)
    } update(state, action, maxQ, reward, overlapRatio)

    currentInput = newDoubleInput
    selectedAction = generateOutput()
  }

  override def produce = selectedAction

  def iterator: Iterator[Boolean] = produce.iterator

  def actionsForInputAt(s: State): Set[Action] =
    allActions//.filter(a => getQValue(s, a) > 0)
}

trait FeatureVectorThalamus extends VecNQLearning with Thalamus {

}

trait Thalamus extends CLA.InputSource {
  type Bit = Int
  type Weight = Double
  type State = Bit
  type Action = Bit
  type StateAction = (State, Action) //input bit, output bit
  type ActionWeight = (Action, Weight)

  def getQ(s: State, a: Action): Weight
  def selector: Selector
  def actionsForInputAt(index: State): Set[Action]
  def minDesiredActivity: Int

  def weightsForInputAt(index: State): Iterator[ActionWeight] = {
    actionsForInputAt(index).iterator.map { action =>
      val q = getQ(index, action)

      action -> q
    }
  }

  def selectAction(weights: Map[State, Weight], num: Int): Set[Action] = {
    selector.streamFrom(weights).take(num).toSet
  }

  def selectAction(weights: Iterable[TraversableOnce[ActionWeight]],
      numWanted: Int, numWeightGroups: Int): Set[Action] = {
    val grouped = weights.flatMap(_.toIterable).groupBy(_._1)

    val sums = for((key, values) <- grouped)
      yield key -> (values.iterator.map(_._2).sum / numWeightGroups.toDouble)

    selectAction(sums, numWanted)
  }

  def selectActionWeighted(input: IndexedSeq[Double]): Set[Action] = {
    val weightsSeq =
      for(idx <- 0 until input.length; overlap = input(idx); if overlap != 0)
      yield weightsForInputAt(idx).map(pair => pair._1 -> (pair._2 * overlap))

    selectAction(weightsSeq, math.max(weightsSeq.length, minDesiredActivity), weightsSeq.length)
  }

  def selectAction(input: CLA.Input): Set[Action] = {
    val weightsSeq =
      for(idx <- 0 until input.length; if input(idx) == true)
        yield weightsForInputAt(idx)

    selectAction(weightsSeq, weightsSeq.length, weightsSeq.length)
  }
}