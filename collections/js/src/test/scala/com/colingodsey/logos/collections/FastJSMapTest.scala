package com.colingodsey.logos.collections

import utest._

import scala.concurrent.duration.Deadline

object FastJSMapTest extends TestSuite {
  val keyMax = 5000
  val keyScale = 2

  val keyMax2 = 100000
  val keyScale2 = 10

  val keys3 = Iterator.from(0).take(keyMax2 * 2).map(_.toString + Seq.fill(100)(" ").mkString).toSeq

  val keys2 = Iterator.continually {
    (math.random * keyMax2 * keyScale2).toInt.toString + Seq.fill(100)(" ").mkString
  }.take(keyMax2).toSeq

  val keys = Iterator.continually {
    (math.random * keyMax * keyScale).toInt.toString
  }.take(keyMax).toSeq

  def timeMe(op: String, fast: => Unit, real: => Unit,
      dict: collection.mutable.Map[String, String] => Unit): Unit = {
    val time = Deadline.now
    fast
    val fastTime = -time.timeLeft.toMillis

    val time2 = Deadline.now
    real
    val realTime = -time2.timeLeft.toMillis

    val time3 = Deadline.now
    dict(scalajs.js.Dictionary.empty[String])
    val nativeTime = -time3.timeLeft.toMillis

    val nativeMutableTime = -time3.timeLeft.toMillis

    val ratio = realTime.toDouble / fastTime * 100 - 100
    val nativeRatio = nativeMutableTime.toDouble / fastTime * 100 - 100

    println(s"op: $op fast map was %$ratio faster (%$nativeRatio faster than native) .")
  }

  //do this so v8 will cache all the hashcodes for the strings

  val testDict = scalajs.js.Dictionary.empty[String]
  keys.foreach(testDict += _ -> "")
  keys2.foreach(testDict += _ -> "")
  keys3.foreach(testDict += _ -> "")


  val tests = TestSuite {
    s"brute force $keyMax" - {

      def commonTest(realMap: Map[String, String], fastMap: FastJSMap[String]) = TestSuite {
        val missingKeys = realMap.keySet -- fastMap.keySet
        val missingKeys2 = fastMap.keySet -- realMap.keySet

        assert {
          realMap.iterator.forall(pair => fastMap.keySet(pair._1)) &&
              fastMap.iterator.forall(pair => realMap.keySet(pair._1))
        }

        assert(realMap.iterator.toSeq.sorted == fastMap.iterator.toSeq.sorted)
        assert(realMap.keys.toSeq.sorted == fastMap.keys.toSeq.sorted)

        require(missingKeys.isEmpty, "missing keys "+ missingKeys)
        require(missingKeys2.isEmpty, "missing keys2 "+ missingKeys2)

        assert(fastMap.keySet.iterator.toSet == realMap.keySet)
        assert(realMap.keySet == fastMap.keySet.iterator.toSet)

      }

      var realMap = Map[String, String]()
      var fastMap = FastJSMap[String]()

      keys.foreach { key =>
        realMap += key -> key
        fastMap += key -> key
      }

      "+ phase" - commonTest(realMap, fastMap)

      var realMap2 = realMap
      var fastMap2 = fastMap

      keys.foreach {
        case key if math.random < 0.5 =>
          realMap2 += key -> key
          fastMap2 += key -> key
        case key =>
          realMap2 -= key
          fastMap2 -= key
      }

      "+/- phase" - commonTest(realMap2, fastMap2)

      var realMap3 = realMap2
      var fastMap3 = fastMap2

      keys.foreach {
        case key if math.random < 0.8 =>
          realMap3 -= key
          fastMap3 -= key
        case key =>
      }

      "- phase" - commonTest(realMap3, fastMap3)

      "timing keys2" - {
        var fastMap = FastJSMap[String]()
        var realMap = Map[String, String]()

        "+" - {
          timeMe("+", keys2.foreach(fastMap += _ -> ""), keys2.foreach(realMap += _ -> ""), x => keys2.foreach(x += _ -> ""))
        }

        "-" - {
          timeMe("-", keys2.foreach(fastMap -= _),keys2.foreach(realMap -= _), x => keys2.foreach(x -= _))
        }

        "+/-" - {
          timeMe("+/-", keys2.foreach {
            case key if math.random < 0.5 =>
              fastMap += key -> ""
            case key =>
              fastMap -= key
          }, keys2.foreach {
            case key if math.random < 0.5 =>
              realMap += key -> ""
            case key =>
              realMap -= key
          }, x => keys2.foreach {
            case key if math.random < 0.5 =>
              x += key -> ""
            case key =>
              x -= key
          })
        }

        "get" - {
          timeMe("get", keys2.foreach(fastMap contains _),keys2.foreach(realMap contains _), x => keys2.foreach(x.contains))
        }
      }

      "timing keys3" - {
        var fastMap = FastJSMap[String]()
        var realMap = Map[String, String]()

        "+" - {
          timeMe("+", keys3.foreach(fastMap += _ -> ""),keys3.foreach(realMap += _ -> ""), x => keys3.foreach(x += _ -> ""))
        }

        "-" - {
          timeMe("-", keys3.foreach(fastMap -= _),keys3.foreach(realMap -= _), x => keys3.foreach(x -= _))
        }

        "+/-" - {
          timeMe("+/-", keys3.foreach {
            case key if math.random < 0.5 =>
              fastMap += key -> ""
            case key =>
              fastMap -= key
          },keys3.foreach {
            case key if math.random < 0.5 =>
              realMap += key -> ""
            case key =>
              realMap -= key
          },x => keys3.foreach {
            case key if math.random < 0.5 =>
              x += key -> ""
            case key =>
              x -= key
          })
        }

        "get" - {
          timeMe("get", keys3.foreach(fastMap contains _),keys3.foreach(realMap contains _), x => keys3.foreach(x.contains))
        }
      }
    }
  }
}



