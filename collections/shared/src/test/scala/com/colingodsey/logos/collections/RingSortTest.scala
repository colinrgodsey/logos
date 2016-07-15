package com.colingodsey.logos.collections

import utest._

import scala.concurrent.duration._
import scala.util.Random

object RingSortTest extends TestSuite {
  def runTrials(trials: Int, maxLength: Int)(trial: Iterator[Double] => Unit) = {
    val rand = new Random(0)

    def generateNumber = rand.nextDouble()
    def generateNumbers(num: Int): Iterator[Double] = Iterator continually generateNumber take num

    for(_ <- 0 until trials) {
      val numbers = generateNumbers((maxLength * rand.nextDouble()).toInt)

      trial(numbers)
    }
  }

  def runUntilTakesTime(minTime: FiniteDuration)(f: Int => Unit): FiniteDuration = {
    val maxMulti = 30
    var multiBits = 1

    def curMulti = 1 << multiBits

    f(1) //1 cold load for class loaded, next warmup hopefully doesnt touch disk

    //warmup
    val warmStart = Deadline.now
    while(Deadline.now < (warmStart + minTime)) {
      f(1)
    }

    while(multiBits < maxMulti) {
      val startTime = Deadline.now

      f(curMulti)

      val diff = -startTime.timeLeft

      if(diff >= minTime) return diff / curMulti

      multiBits += 1
    }

    sys.error("tests dont take enough time! multiplier got above " + maxMulti)
  }

  val tests = TestSuite {
    val nTrials = 100
    val maxLength = 50000

    def nUnique = maxLength / 2 //even amount of dupes
    def toTake = maxLength / 100

    "compare to sort 5" - runTrials(nTrials, maxLength) { numbersItr =>
      val numbers = numbersItr.toVector

      val ringTop = RingSort.takeTop(5, numbers).toVector
      val sortTop = numbers.sorted.reverse.take(5)

      assert(ringTop == sortTop)
    }

    "compare to sort 200" - runTrials(nTrials, maxLength) { numbersItr =>
      val numbers = numbersItr.toVector

      val ringTop = RingSort.takeTop(200, numbers).toVector
      val sortTop = numbers.sorted.reverse.take(200)

      assert(ringTop == sortTop)
    }

    s"compare to sort int dup check" - runTrials(nTrials, maxLength) { numbersItr =>
      val numbers = numbersItr.toVector.map(x => (x * nUnique).toInt)

      val ringTop = RingSort.takeTop(toTake, numbers).toVector
      val sortTop = numbers.sorted.reverse.take(toTake)

      assert(ringTop == sortTop)
    }

    "compare to sort int 20 (0-100)" - runTrials(nTrials, maxLength) { numbersItr =>
      val numbers = numbersItr.toVector.map(x => (x * 100).toInt)

      val ringTop = RingSort.takeTop(20, numbers).toVector
      val sortTop = numbers.sorted.reverse.take(20)

      assert(ringTop == sortTop)
    }

    "compare to sort string 20" - runTrials(nTrials, maxLength) { numbersItr =>
      val numbers: Seq[String] = numbersItr.toVector.map(_.toString)

      val ringTop = RingSort.takeTop(20, numbers).toVector
      val sortTop = numbers.sorted.reverse.take(20)

      assert(ringTop == sortTop)
    }

    "compare to sort tuple 20" - runTrials(nTrials, maxLength) { numbersItr =>
      val numbers: Seq[(Double, String)] = numbersItr.toVector.map(x => (x, x.toString))

      val ringTop = RingSort.takeTop(20, numbers).toVector
      val sortTop = numbers.sorted.reverse.take(20)

      assert(ringTop == sortTop)
    }

    //this doesnt work on js somehow
    "speed test 200" - {
      val nTrials = 10
      val maxLength = 50000
      val take = 1000

      val sortTime = runUntilTakesTime(1.seconds) { mult =>
        runTrials(nTrials * mult, maxLength) { numbersItr =>
          val numbers = numbersItr.toArray

          numbers.sorted.take(100)
        }
      }

      val ringTime = runUntilTakesTime(1.seconds) { mult =>
        runTrials(nTrials * mult, maxLength) { numbersItr =>
          val numbers = numbersItr.toArray

          RingSort.takeTop(100, numbers).toArray
        }
      }

      val perc = (sortTime.toNanos.toDouble / ringTime.toNanos - 1) * 100.0

      perc + "% faster ring sort time"
    }
  }
}
