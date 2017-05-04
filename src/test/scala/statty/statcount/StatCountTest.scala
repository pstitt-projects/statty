package statty.statcount

import grizzled.math.stats._
import org.scalatest.Matchers

/**
  * Created by Paul Stitt on 01/05/2017.
  */
class StatCountTest extends org.scalatest.FlatSpec with Matchers {

  val TOLERANCE = 0.000000001

  behavior of "Initialisation of StatCount"

  it should "get the correct count of values" in {
    StatCount(1, 2, 4, 8).count shouldBe 4
  }

  it should "get the correct count of anti-values" in {
    StatCount.ofAntiValues(1, 2, 4, 8).count shouldBe -4
  }

  it should "get the correct mean of values" in {
    StatCount(1, 2, 4, 8).mean.doubleValue() shouldBe mean(1, 2, 4, 8) +- TOLERANCE
  }

  it should "get the correct variance of values" in {
    StatCount(1, 2, 4, 8).sampleVariance.doubleValue() shouldBe sampleVariance(1, 2, 4, 8) +- TOLERANCE
  }

  it should "handle zero count" in {
    val statCount = StatCount(9) - 5
    statCount.count shouldBe 0
    statCount.mean.doubleValue() shouldBe 0.0 +- TOLERANCE
    statCount.sampleVariance.doubleValue() shouldBe 0.0 +- TOLERANCE
  }

  behavior of "Subtraction of values to StatCount"

  it should "decrement count" in {
    (StatCount(1,9) - 1).count shouldBe 1
  }

  it should "calculate mean" in {
    (StatCount(1,9,6) - 1).mean.doubleValue() shouldBe 7.5 +- TOLERANCE
  }

  it should "calculate sampleVariance" in {
    (StatCount(1,9,6) - 1).sampleVariance.doubleValue() shouldBe sampleVariance(6, 9) +- TOLERANCE
  }

  behavior of "Addition of StatCounts"

  it should "add counts of values commutatively" in {
    StatCount(1,5) + StatCount(1,9) shouldBe StatCount(1,5,1,9)
    StatCount(1,9) + StatCount(1,5) shouldBe StatCount(1,5,1,9)
  }

  it should "add counts of values associatively" in {
    (StatCount(1,5) + StatCount(1,9)) + StatCount(1,9,6) shouldBe StatCount(1,5) + (StatCount(1,9) + StatCount(1,9,6))
  }

  it should "add counts commutatively with a mixture of values and anti-values" in {
    StatCount(1, 2) - 2 + 5 + (StatCount(9) - 5) shouldBe StatCount(1, 2) - 2 + 5 + (StatCount(9) - 5)
    StatCount(1, 2) - 2 + 5 + (StatCount(9) - 5) shouldBe StatCount(1,9)
    (StatCount(9) - 5) + StatCount(1, 2) - 2 + 5 shouldBe StatCount(1, 2) - 2 + 5 + (StatCount(9) - 5)
    (StatCount(9) - 5) + StatCount(1, 2) - 2 + 5 shouldBe StatCount(1,9)
  }

  it should "cancel values and anti-values" in {
    StatCount(1) + StatCount(2) + StatCount.ofAntiValues(2) shouldBe StatCount(1)
    (StatCount(1) + StatCount(2)) + (StatCount.ofAntiValues(2) + StatCount(5)) + (StatCount.ofAntiValues(5) + StatCount(9)) shouldBe (StatCount(1) + StatCount(9))
  }

  it should "not treat negative values as anti-values" in {
    (StatCount(2) + StatCount(-2)).count shouldBe 2
  }

  it should "treat a count that is added and subtracted as a no-op" in {
    (StatCount(1) + StatCount(2)) - StatCount(2) shouldBe StatCount(1)
  }

  behavior of "Subtraction of StatCounts"

  it should "subtract counts of values leaving no trace" in {
    StatCount(1,5,9) - StatCount(1,5) shouldBe StatCount(9)
  }
}


