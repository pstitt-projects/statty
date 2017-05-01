package com.rentalcars.analytics.stats

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object NonSingularComplexStatCount {
  val MATCH_TOLERANCE = 0.00000001
}

case class NonSingularComplexStatCount(count0: Int = 0,
                                       mean0: BigDecimal = 0,
                                       M20: BigDecimal = 0)
  extends ComplexStatCount {
  override def count: Int = count0

  override def mean: BigDecimal = mean0

  override def M2: BigDecimal = M20

  override def +(value: BigDecimal): ComplexStatCount = {
    // println(s"${this} + ${value}")
    ComplexStatAlgorithms.mergeUsingWelford(this, value)
  }

  override def +(other: ComplexStatCount): ComplexStatCount = {
    // println(s"${this} + ${other}")
    other match {
      case singularity: Singularity => {
        val na = singularity.a.n
        val nb = singularity.b.n
        if (n + na == 0) {
          (this + singularity.b) + singularity.a
        } else {
          (this + singularity.a) + singularity.b
        }
      }
      case stat: NonSingularComplexStatCount =>
        ComplexStatAlgorithms.mergeUsingChan(this, stat)
    }
  }

  override def -(value: BigDecimal): ComplexStatCount = {
    //  println(s"${this} - ${value}")
    ComplexStatAlgorithms.unmergeUsingWelford(this, value)
  }

  override def -(other: ComplexStatCount): ComplexStatCount = {
    //  println(s"${this} - ${other}")
    other match {
      case singularity: Singularity => {
        val na = singularity.a.n
        val nb = singularity.b.n
        if (n - na == 0) {
          (this - singularity.b) - singularity.a
        } else {
          (this - singularity.a) - singularity.b
        }
      }
      case stat: NonSingularComplexStatCount =>
        ComplexStatAlgorithms.unmergeUsingChan(this, stat)
    }
  }

  override def toString(): String = {
    s"ComplexStatCount[n=${n}, mean=${mean}, M2=${M2}, pVar=${populationVariance}, sVar=${sampleVariance}]"
    //    s"ComplexStatCount[n=${n}, mean=${bdToString(mean)}, M2=${bdToString(M2)}, pVar=${
    //      bdToString(
    //        populationVariance)
    //    }, sVar=${bdToString(sampleVariance)}]"
  }

  def canEqual(a: Any) = a.isInstanceOf[NonSingularComplexStatCount]

  override def equals(that: Any): Boolean =
    that match {
      case that: NonSingularComplexStatCount => {
        that.canEqual(this) &&
          matches(n, that.n) &&
          matches(mean, that.mean) &&
          matches(M2, that.M2)
      }
      case _ => false
    }

  private def matches(a: BigDecimal, b: BigDecimal) : Boolean = {
    var diff = if (a>b) a-b else b-a
    diff < NonSingularComplexStatCount.MATCH_TOLERANCE
  }
}

