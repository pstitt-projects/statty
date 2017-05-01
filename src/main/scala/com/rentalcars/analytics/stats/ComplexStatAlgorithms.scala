package com.rentalcars.analytics.stats

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object ComplexStatAlgorithms {
  def mergeUsingChan(a: NonSingularComplexStatCount,
                     b: NonSingularComplexStatCount): ComplexStatCount = {
    val n = a.n + b.n
    if (n == 0) {
      //      println(s"merge counter ${a} with ${b} = Zero")
      new Singularity(a, b)
    } else {
      val d = b.mean - a.mean
      val mean = (a.mean * a.n + b.mean * b.n) / n
      val M2 = a.M2 + b.M2 + (d.pow(2) * a.n * b.n) / n
      //      println(s"merge counter ${a} with ${b} = [${n}, ${mean}, ${M2}]")
      ComplexStatCount(n, mean, M2)
    }
  }

  def unmergeUsingChan(a: NonSingularComplexStatCount,
                       b: NonSingularComplexStatCount): ComplexStatCount = {
    mergeUsingChan(a, NonSingularComplexStatCount(b.n * (-1), b.mean, b.M2))
  }

  def mergeUsingWelford(a: NonSingularComplexStatCount,
                        value: BigDecimal): ComplexStatCount = {
    val n = a.n + 1

    if (n == 0) {
      //      println(s"merge value ${value} with ${a} = Zero")
      Singularity(
        a.asInstanceOf[NonSingularComplexStatCount],
        ComplexStatCount.ofValues(value).asInstanceOf[NonSingularComplexStatCount])
    } else {
      val d = value - a.mean
      val mean = a.mean + (d / n)
      val d2 = value - mean
      val M2 = a.M2 + d * d2
      //      println(s"merge value ${value} with ${a} = [${n}, ${mean}, ${M2}]")
      ComplexStatCount(n, mean, M2)
    }
  }

  def unmergeUsingWelford(a: NonSingularComplexStatCount,
                          value: BigDecimal): ComplexStatCount = {
    val n = a.n - 1

    if (n == 0) {
      //      println(s"unmerge value ${value} from ${a} = Zero")
      new Singularity(
        a,
        (ComplexStatCount() - value).asInstanceOf[NonSingularComplexStatCount])
    } else {
      val d = value - a.mean
      val mean = a.mean - (d / n)
      val d2 = value - mean
      val M2 = a.M2 - d * d2
      //      println(s"unmerge value ${value} from ${a} = [${n}, ${mean}, ${M2}]")
      ComplexStatCount(n, mean, M2)
    }
  }
}

