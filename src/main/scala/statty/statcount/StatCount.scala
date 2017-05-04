package statty.statcount

/**
  * Created by Paul Stitt on 01/05/2017.
  */

sealed trait StatCount {
  def count: Int

  def nonEmpty: Boolean = (count != 0)

  def n: Int = count

  def mean: BigDecimal

  def sum: BigDecimal = count*mean

  def M2: BigDecimal

  def +(value: BigDecimal): StatCount

  def +(other: StatCount): StatCount

  def -(value: BigDecimal): StatCount

  def -(other: StatCount): StatCount

  def populationVariance: BigDecimal = {
    if (n > 0) {
      M2 / n
    } else {
      0
    }
  }

  def sampleVariance: BigDecimal = {
    if (n > 1) {
      M2 / (n - 1)
    } else {
      0
    }
  }
}

object StatCount {
  def apply(values: BigDecimal*) : StatCount = {
    ofValues(values: _*)
  }

  def ofValues(valuesToAdd: BigDecimal*): StatCount = {
    var stat : StatCount = Empty
    valuesToAdd.foreach(value => {
      stat = stat + value
    })
    stat
  }

  def ofAntiValues(valuesToAdd: BigDecimal*): StatCount = {
    var stat : StatCount = Empty
    valuesToAdd.foreach(value => {
      stat = stat - value
    })
    stat
  }

  def fromStats(count: Int, mean: BigDecimal, M2: BigDecimal) : StatCount = {
    if (count == 0) {
      Empty
    }
    else {
      Real(count, mean, M2)
    }
  }

  private val MATCH_TOLERANCE = 0.00000001

  object Empty extends Real(0, 0, 0)

  private[StatCount] case class Real(count0: Int, mean0: BigDecimal, M20: BigDecimal) extends StatCount {
    override def count: Int = count0

    override def mean: BigDecimal = mean0

    override def M2: BigDecimal = M20

    override def +(value: BigDecimal): StatCount = {
      Algorithms.mergeUsingWelford(this, value)
    }

    override def +(other: StatCount): StatCount = {
      other match {
        case imaginary: Imaginary => {
          val na = imaginary.a.n
          val nb = imaginary.b.n
          if (n + na == 0) {
            (this + imaginary.b) + imaginary.a
          } else {
            (this + imaginary.a) + imaginary.b
          }
        }
        case stat: Real =>
          Algorithms.mergeUsingChan(this, stat)
      }
    }

    override def -(value: BigDecimal): StatCount = {
      Algorithms.unmergeUsingWelford(this, value)
    }

    override def -(other: StatCount): StatCount = {
      other match {
        case imaginary: Imaginary => {
          val na = imaginary.a.n
          val nb = imaginary.b.n
          if (n - na == 0) {
            (this - imaginary.b) - imaginary.a
          } else {
            (this - imaginary.a) - imaginary.b
          }
        }
        case stat: Real =>
          Algorithms.unmergeUsingChan(this, stat)
      }
    }

    override def toString(): String = {
      def format(value: BigDecimal): String = {
        s"${value.setScale(2, BigDecimal.RoundingMode.HALF_UP)}"
      }

      s"StatCount[n=${n}, mean=${format(mean)}, M2=${format(M2)}, pVar=${format(populationVariance)}, sVar=${format(sampleVariance)}]"
    }

    def canEqual(a: Any) = a.isInstanceOf[Real]

    override def equals(that: Any): Boolean =
      that match {
        case that: Real => {
          that.canEqual(this) &&
            matches(n, that.n) &&
            matches(mean, that.mean) &&
            matches(M2, that.M2)
        }
        case _ => false
      }

    override def hashCode(): Int = {
      n + mean.toInt + M2.toInt
    }

    private def matches(a: BigDecimal, b: BigDecimal) : Boolean = {
      var diff = if (a>b) a-b else b-a
      diff < MATCH_TOLERANCE
    }
  }

  private[StatCount] case class Imaginary(a: Real,
                       b: Real)
    extends StatCount {

    lazy val count: Int = 0
    lazy val mean: BigDecimal = 0
    lazy val M2: BigDecimal = 0

    override def +(value: BigDecimal): StatCount = {
      if (a.n != -1) {
        (a + value) + b
      } else {
        (b + value) + a
      }
    }

    override def +(other: StatCount): StatCount = {
      if (a.n + other.n != 0) {
        (a + other) + b
      } else {
        (b + other) + a
      }
    }

    override def -(value: BigDecimal): StatCount = {
      if (a.n != 1) {
        (a - value) + b
      } else {
        (b - value) + a
      }
    }

    override def -(other: StatCount): StatCount = {
      if (a.n - other.n != 0) {
        (a - other) + b
      } else {
        (b - other) + a
      }
    }

    override def toString(): String = {
      s"Imaginary[a=${a}, b=${b}]"
    }
  }

  object Algorithms {
    def mergeUsingChan(a: Real,
                       b: Real): StatCount = {
      val n = a.n + b.n
      if (n == 0) {
        new Imaginary(a, b)
      } else {
        val d = b.mean - a.mean
        val mean = (a.mean * a.n + b.mean * b.n) / n
        val M2 = a.M2 + b.M2 + (d.pow(2) * a.n * b.n) / n
        StatCount.fromStats(n, mean, M2)
      }
    }

    def unmergeUsingChan(a: Real,
                         b: Real): StatCount = {
      mergeUsingChan(a, Real(b.n * (-1), b.mean, -1*b.M2))
    }

    def mergeUsingWelford(a: Real,
                          value: BigDecimal): StatCount = {
      val n = a.n + 1

      if (n == 0) {
        Imaginary(
          a.asInstanceOf[Real],
          StatCount.ofValues(value).asInstanceOf[Real])
      } else {
        val d = value - a.mean
        val mean = a.mean + (d / n)
        val d2 = value - mean
        val M2 = a.M2 + d * d2
        StatCount.fromStats(n, mean, M2)
      }
    }

    def unmergeUsingWelford(a: Real,
                            value: BigDecimal): StatCount = {
      val n = a.n - 1

      if (n == 0) {
        new Imaginary(
          a,
          (StatCount.fromStats(0, 0, 0) - value).asInstanceOf[Real])
      } else {
        val d = value - a.mean
        val mean = a.mean - (d / n)
        val d2 = value - mean
        val M2 = a.M2 - d * d2
        StatCount.fromStats(n, mean, M2)
      }
    }
  }
}
