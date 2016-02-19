package com.colingodsey.logos.cla

import scala.annotation.tailrec

object NewCLA {
  trait NewTopology {
    
  }

  trait LayerCellData {
    def numColumns: Int
    def columnHeight: Int
    def numCells: Int = numColumns * columnHeight

    val cellActiveData = new Array[Boolean](numCells)
    //val cellPredictiveData = new Array[Boolean](numCells)
    val columnActiveData = new Array[Boolean](numColumns)
    val columnActiveDutyCycleData = new Array[Double](numColumns)

    def columnActiveDutyCycleOf(idx: Int): Double = columnActiveDutyCycleData(idx)
    def cellActiveOf(idx: Int): Boolean
    def columnActiveOf(idx: Int): Boolean
  }
  
  trait Layer { //will be a class!



  }

  trait SpatialPooler {
    def dynamicInhibitionRadius: Boolean
    def inhibitionRadius: Double
    def inhibitionRadiusIsLarge: Boolean
    def desiredLocalActivity: Int

    def numItems: Int

    def activeDutyCycleOf(idx: Int): Double

    def localSpatialPooler(idx: Int): Unit = {
      ???
    }

    protected def spatialPooler(): Unit = {
      var maxDutyCycle = 0.0

      if(dynamicInhibitionRadius && !inhibitionRadiusIsLarge) {
        var idx = 0
        while(idx < numItems) {
          val activeDutyCycle = activeDutyCycleOf(idx)

          if(activeDutyCycle > maxDutyCycle)
            maxDutyCycle = activeDutyCycle

          localSpatialPooler(idx)

          idx += 1
        }
      } else {
        //get top k and find minimum, filter based on below minimum for deactivation


        val sorted = segments.sortBy(_.activationOrdinal).toStream.reverse
        val (topK, tail) = sorted.splitAt(desiredLocalActivity)

        maxDutyCycle = segments.maxBy(_.activeDutyCycle.toDouble).activeDutyCycle.toDouble

        //activated top columns within our inhibition radius
        tail.foreach(_.deactivate())
        topK.foreach {
          case x if x.overlap > 0 => x.activate()
          case x => x.deactivate()
        } //only active inputs
      }

      //update rolling averages
      segments.foreach(_.updateDutyCycle(maxDutyCycle, force = true))
    }
  }
}
