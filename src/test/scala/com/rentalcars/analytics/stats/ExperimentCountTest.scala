package com.rentalcars.analytics.stats

import grizzled.math.stats._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Paul on 01/05/2017.
  */
class ExperimentCountTest extends FlatSpec with Matchers {

  val TOLERANCE = 0.000000001

  behavior of "ExperimentCount"

  val stat19 = ComplexStatCount.ofValues(1, 9)
  val stat25 = ComplexStatCount.ofValues(2, 5)
  val ec25 = ExperimentCount(1000, stat25)
  val ec19 = ExperimentCount(700, stat19)

  it should "add counts" in {
    val testCount = ec25 + ec19

    testCount.impressions shouldBe 1700
    testCount.goalCount.count shouldBe 4
    testCount.goalCount.mean.doubleValue() shouldBe mean(1,2,5,9) +- TOLERANCE
    testCount.goalCount.sampleVariance.doubleValue() shouldBe sampleVariance(1,2,5,9) +- TOLERANCE
  }

  it should "subtract counts" in {
    val testCount = ec25 - 2

    testCount.impressions shouldBe 1000
    testCount.goalCount.count shouldBe 1
    testCount.goalCount.mean.doubleValue() shouldBe 5.0 +- TOLERANCE
    testCount.goalCount.sampleVariance.doubleValue() shouldBe 0.0 +- TOLERANCE
  }
}
