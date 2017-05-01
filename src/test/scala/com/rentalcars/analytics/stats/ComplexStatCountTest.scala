package com.rentalcars.analytics.stats

import grizzled.math.stats._
import org.scalatest.Matchers

/**
  * Created by Paul Stitt on 01/05/2017.
  */
class ComplexStatCountTest extends org.scalatest.FlatSpec with Matchers {

  val TOLERANCE = 0.000000001

  behavior of "Initialisation of ComplexStatCount"

  val count1248 = ComplexStatCount.ofValues(1, 2, 4, 8)
  val count1248m = ComplexStatCount.ofAntiValues(1, 2, 4, 8)
  val count19 = ComplexStatCount.ofValues(1, 9)
  val count196 = ComplexStatCount.ofValues(1, 9, 6)
  val count15 = ComplexStatCount.ofValues(1, 5)
  val count1225 = ComplexStatCount.ofValues(1, 2) - 2 + 5
  val count59 = ComplexStatCount.ofValues(9) - 5
  val count122559 = count1225 + count59
  val count591225 = count59 + count1225

  it should "get the correct count of values" in {
    count1248.count shouldBe 4
  }

  it should "get the correct count of anti-values" in {
    count1248m.count shouldBe -4
  }

  it should "get the correct mean of values" in {
    count1248.mean.doubleValue() shouldBe mean(1, 2, 4, 8) +- TOLERANCE
  }

  it should "get the correct variance of values" in {
    count1248.sampleVariance.doubleValue() shouldBe sampleVariance(1, 2, 4, 8) +- TOLERANCE
  }

  it should "handle zero count" in {
    count59.count shouldBe 0
    count59.mean.doubleValue() shouldBe 0.0 +- TOLERANCE
    count59.sampleVariance.doubleValue() shouldBe 0.0 +- TOLERANCE
  }

  behavior of "Subtraction of values to ComplexStatCount"

  it should "decrement count" in {
    (count19 - 1).count shouldBe 1
  }

  it should "calculate mean" in {
    (count196 - 1).mean.doubleValue() shouldBe 7.5 +- TOLERANCE
  }

  it should "calculate sampleVariance" in {
    (count196 - 1).sampleVariance.doubleValue() shouldBe sampleVariance(6, 9) +- TOLERANCE
  }

  behavior of "Addition of ComplexStatCounts"

  it should "add counts of values commutatively" in {
    count15 + count19 shouldBe ComplexStatCount.ofValues(1,5,1,9)
    count19 + count15 shouldBe ComplexStatCount.ofValues(1,5,1,9)
  }

  it should "add counts of values associatively" in {
    (count15 + count19) + count196 shouldBe count15 + (count19 + count196)
  }

  it should "add counts commutatively with a mixture of values and anti-values" in {
    count1225 + count59 shouldBe count122559
    count1225 + count59 shouldBe count19
    count59 + count1225 shouldBe count122559
    count59 + count1225 shouldBe count19
  }

  val c1 = ComplexStatCount.ofValues(1)
  val c2 = ComplexStatCount.ofValues(2)
  val cm2 = ComplexStatCount.ofValues(-2)
  val c2n = ComplexStatCount.ofAntiValues(2)
  val c5 = ComplexStatCount.ofValues(5)
  val c5n = ComplexStatCount.ofAntiValues(5)
  val c9 = ComplexStatCount.ofValues(9)

  it should "cancel values and anti-values" in {
    c1 + c2 + c2n shouldBe c1
    (c1 + c2) + (c2n + c5) + (c5n + c9) shouldBe (c1 + c9)
  }

  it should "not treat negative values as anti-values" in {
    (c2 + cm2).count shouldBe 2
  }

  it should "treat a count that is added and subtracted as a no-op" in {
    (c1 + c2) - c2 shouldBe c1
  }
}


