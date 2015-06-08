package com.colingodsey.logos.cla

trait SDR extends DutyCycle {
  def connectionThreshold: Double
  def permanenceInc: Double
  def permanenceDec: Double
  def minDistalPermanence: Double

  protected var connections: Array[NodeAndPermanence]

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

  def addConnection(node: NeuralNode, p: Double) = {
    node.connectOutput()
    connections :+= new NodeAndPermanence(node, p)
  }

  def pruneSynapses(): Int = if(needsPruning) {
    var pruned = 0

    connections = connections filter {
      case np if np.p < minDistalPermanence =>
        pruned += 1
        np.node.disconnectOutput()
        false
      case _ => true
    }

    pruned
  } else 0
}
