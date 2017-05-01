package com.rentalcars.analytics.stats.demo

import com.rentalcars.analytics.stats.ComplexStatCount

import grizzled.math.stats._

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object ComplexStatCountDemo {
  def main(args: Array[String]) = {

    val stat19 = ComplexStatCount.ofValues(1, 9)

    println(
      s"(1,9) = ${stat19}, expected (2, ${mean(1, 9)}, ${populationVariance(1, 9)})")

    val stat15 = ComplexStatCount.ofValues(1, 5)

    println(
      s"(1,5) = ${stat15}, expected (2, ${mean(1, 5)}, ${populationVariance(1, 5)})")

    val stat1225 = ComplexStatCount.ofValues(1, 2) - 2 + 5
    println(s"(1,2,-2,5) = ${stat1225}")

    val stat59 = ComplexStatCount.ofValues(9) - 5
    println(s"(-5,9) = ${stat59}")

    val stat122559 = stat1225 + stat59
    println(s"(1,2,-2,5) + (-5,9) = ${stat122559}")

    val stat591225 = stat59 + stat1225
    println(s"(-5,9) + (1,2,-2,5) = ${stat591225}")

    val c1 = ComplexStatCount.ofValues(1)
    val c2 = ComplexStatCount.ofValues(2)
    val cm2 = ComplexStatCount.ofValues(-2)
    val c2n = ComplexStatCount.ofAntiValues(2)
    val c5 = ComplexStatCount.ofValues(5)
    val c5n = ComplexStatCount.ofAntiValues(5)
    val c9 = ComplexStatCount.ofValues(9)

    if (c1 + c2 + c2n != c1) println("Fail 1") else println("Pass 1")
    if ((c2 + cm2).n != 2) println("Fail 2") else println("Pass 2")
    if ((c1 + c2) + (c2n + c5) + (c5n + c9) != (c1 + c9)) println("Fail 3")
    else println("Pass 3")

    val stat1259 = ComplexStatCount.ofValues(1, 2, 5, 9)
    if (stat1259.populationVariance != populationVariance(1, 2, 5, 9))
      println(
        s"Fail 4 - ${stat1259.populationVariance}, ${populationVariance(1, 2, 5, 9)}")
    else println("Pass 4")

    if ((c1 + c2) - c2 != c1) println("Fail 5") else println("Pass 5")
  }
}
