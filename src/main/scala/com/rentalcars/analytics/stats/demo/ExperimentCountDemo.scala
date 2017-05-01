package com.rentalcars.analytics.stats.demo

import com.rentalcars.analytics.stats.{ComplexStatCount, ExperimentCount}

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object ExperimentCountDemo {
  def main(args: Array[String]) = {

    val stat19 = ComplexStatCount.ofValues(1, 9)
    val stat1259 = ComplexStatCount.ofValues(1, 2, 5, 9)

    val ec1 = ExperimentCount(1000, stat1259)
    val ec2 = ExperimentCount(700, stat19)
    println(s"${ec1} + ${ec2} = ${ec1 + ec2}")
  }
}
