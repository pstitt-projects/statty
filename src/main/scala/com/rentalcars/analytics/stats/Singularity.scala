package com.rentalcars.analytics.stats

/**
  * Created by Paul Stitt on 01/05/2017.
  */
case class Singularity(a: NonSingularComplexStatCount,
                       b: NonSingularComplexStatCount)
  extends ComplexStatCount {

  lazy val count: Int = 0
  lazy val mean: BigDecimal = 0
  lazy val M2: BigDecimal = 0

  override def +(value: BigDecimal): ComplexStatCount = {
    //    println(s"zero plus value: ${a} + ${value} + ${b}")
    if (a.n != -1) {
      (a + value) + b
    } else {
      (b + value) + a
    }
  }

  override def +(other: ComplexStatCount): ComplexStatCount = {
    //    println(s"zero plus stat: ${a} + ${other} + ${b}")
    if (a.n + other.n != 0) {
      (a + other) + b
    } else {
      (b + other) + a
    }
  }

  override def -(value: BigDecimal): ComplexStatCount = {
    //    println(s"zero minus value: ${a} - ${value} + ${b}")
    if (a.n != 1) {
      (a - value) + b
    } else {
      (b - value) + a
    }
  }

  override def -(other: ComplexStatCount): ComplexStatCount = {
    //    println(s"zero minus stat: ${a} - ${other} + ${b}")
    if (a.n - other.n != 0) {
      (a - other) + b
    } else {
      (b - other) + a
    }
  }

  override def toString(): String = {
    s"Singularity[a=${a}, b=${b}]"
  }
}

