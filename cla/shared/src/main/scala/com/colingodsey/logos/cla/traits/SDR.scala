package com.colingodsey.logos.cla.traits

import com.colingodsey.logos.cla.{NeuralNode, NodeAndPermanence}

trait SDR extends DutyCycle {
  def connectionThreshold: Double
  def permanenceInc: Double
  def permanenceDec: Double
  def minDistalPermanence: Double

  protected var connections: IndexedSeq[NodeAndPermanence]

  //private var connectedTo = Set[NeuralNode]()

  def numConnections = connections.length

  def boostPermanence(): Unit = {
    for(i <- 0 until connections.length) {
      val np = connections(i)

      np.p = (np.p + connectionThreshold * 0.1 * math.random)
    }
  }

  //TODO: minActivation?
  def reinforce(): Unit = /*if(activation > minActivation)*/ {
    for(i <- 0 until connections.length) {
      val np = connections(i)

      val newP =
        if (np.node.active) math.min(1.0, np.p + permanenceInc)
        else math.max(0.0, np.p - permanenceDec)

      if(newP != np.p)
        np.p = newP
    }
  }

  //TODO: merge into reinforce?
  def needsPruning = connections.exists {
    case np if np.p < minDistalPermanence => true
    case _ => false
  }

  def connectedTo(n: NeuralNode) = connections.exists(_.node == n)

  def addConnection(node: NeuralNode, p: Double) = if(!connectedTo(node)) {
    node.connectOutput()
    connections :+= new NodeAndPermanence(node, p)
    //connectedTo += node
  }

  private def removeConnection0(node: NeuralNode): Unit = if(connectedTo(node)) {
    //connectedTo -= node
    node.disconnectOutput()
  }

  def removeConnection(node: NeuralNode): Boolean = if(connectedTo(node)) {
    connections = connections.filter(_.node != node)
    removeConnection0(node)

    true
  } else false

  def removeAllConnections(): Unit =
    connections.foreach(x => removeConnection(x.node))

  def pruneSynapses(): Int = if(needsPruning) {
    var pruned = 0

    connections = connections filter {
      case np if np.p < minDistalPermanence =>
        pruned += 1
        removeConnection0(np.node)
        false
      case _ => true
    }

    pruned
  } else 0
}
