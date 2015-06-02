package com.colingodsey.logos.cla


class Region(implicit val config: CLA.Config) { region =>
  import com.colingodsey.logos.cla.CLA._
  import config._

  val columns = (0 until regionWidth).map(new Column(region, _)).toVector
  val input = Array.fill(inputWidth)(false)

  var inhibitionRadius: Radius = 1.0

  def update(input: Input): Unit = {
    for(i <- 0 until input.length) this.input(i) = input(i)

    spatialPooler()
    columns.foreach(_.temporalPrePooler())
    columns.foreach(_.temporalPostPooler())
  }

  def columnsNear(loc: Location, rad: Radius) = {
    val min = math.max(0, loc - rad).toInt
    val max = math.min(regionWidth - 1, loc + rad).toInt

    (min to max).iterator map columns
  }

  def spatialPooler(): Unit = {
    //clear activation state and update input
    columns.foreach { column =>
      column.active = false
      column.proximalDendrite.update()
    }

    //TODO: real inhibition radius?
    val sorted = columns.sortBy(-_.overlap)
    val (topK, tail) = sorted.splitAt(desiredLocalActivity)

    //activated top columns within our inhibition radius
    topK.filter(_.overlap > 0).foreach(_.activate())

    //update rolling averages
    columns.foreach(_.updateDutyCycle())

    inhibitionRadius = averageReceptiveFieldSize / inputConnectionsPerColumn * regionWidth / 2.0
  }

  def averageReceptiveFieldSize = {
    val sum = columns.map(_.receptiveFieldSize).sum

    sum.toDouble / columns.length
  }

  def seedDistalSynapses(): Unit = {
    columns.foreach(_.seedDistalSynapses())
  }

  def getRandomCell(refColumn: Column): NeuralNode = {
    val column = columns((columns.length * math.random).toInt)

    //TODO: ignore same column... or no?
    if(column == refColumn) getRandomCell(refColumn)
    else column.cells((column.cells.length * math.random).toInt)
  }

  def getRandomPermanence = {
    val s = connectionThreshold * 0.1 //10% variance
    val n = (s * 2 * math.random) - s

    connectionThreshold + n
  }
}
