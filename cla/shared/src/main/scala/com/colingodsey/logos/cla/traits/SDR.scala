package com.colingodsey.logos.cla.traits

import com.colingodsey.logos.cla.{NeuralNode, NodeAndPermanence}

trait SDR extends { //DutyCycle {
  def connectionThreshold: Double
  def permanenceInc: Double
  def permanenceDec: Double
  def minDistalPermanence: Double

  protected val connectionBuffer = collection.mutable.Buffer[NodeAndPermanence]()
  protected var _numConnections = 0
  protected val connectionIndex = collection.mutable.Map[NeuralNode, Int]()

  def connections = connectionBuffer.iterator.filter(_ != null)

  def numConnections = _numConnections

  //private var connectedTo = Set[NeuralNode]()

  def boostPermanence(): Unit = {
    var i = 0
    while(i < connectionBuffer.length) {
      val np = connectionBuffer(i)
      if(np != null)
        np.p = (np.p + connectionThreshold * 0.1 * math.random * 2.0)

      i += 1
    }
  }

  //TODO: minActivation?
  def reinforce(): Unit = /*if(activation > minActivation)*/ {
    var i = 0
    while(i < connectionBuffer.length) {
      val np = connectionBuffer(i)
      if(np != null) {
        val oldP = np.p
        val newP =
          if (np.node.active) math.min(1.0, oldP + permanenceInc * math.random * 2.0)
          else math.max(0.0, oldP - permanenceDec * math.random * 2.0)

        if (newP != oldP)
          np.p = newP
      }

      i += 1
    }
  }

  //TODO: merge into reinforce?
  def needsPruning = connections.exists {
    case np if np.p < minDistalPermanence => true
    case _ => false
  }

  def connectedTo(n: NeuralNode) = connectionIndex contains n

  def addConnection(node: NeuralNode, p: Double): Unit =
    addConnection(new NodeAndPermanence(node, p))

  def addConnection(np: NodeAndPermanence): Unit = if(!connectedTo(np.node)) {
    val node = np.node
    val p = np.p

    node.connectOutput()

    var idx = 0
    var found = false
    while(!found && idx < connectionBuffer.length) {
      connectionBuffer(idx) match {
        case null =>
          found = true
          connectionBuffer(idx) = np
          connectionIndex += node -> idx
        case _ =>
          idx += 1
      }
    }

    if(!found) {
      connectionIndex += node -> (connectionBuffer.length)
      connectionBuffer.append(np)
    }

    _numConnections += 1
  }

  def removeConnection(node: NeuralNode): Boolean = connectionIndex.get(node) match {
    case None => false
    case Some(idx) =>
      connectionBuffer(idx) = null
      node.disconnectOutput()
      _numConnections -= 1
      connectionIndex -= node
      true
  }

  def removeAllConnections(): Unit = {
    connections.foreach(x => removeConnection(x.node))
    connectionBuffer.clear()
    connectionIndex.clear()
    _numConnections = 0
  }


  def pruneSynapses(): Int = if(needsPruning) {
    var pruned = 0

    connections foreach {
      case np if np.p < minDistalPermanence =>
        pruned += 1
        removeConnection(np.node)
        false
      case _ => true
    }

    pruned
  } else 0
}
