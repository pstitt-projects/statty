package statty.statcount.demo

import statty.statcount.ComplexStatCount

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object ComplexStatCountDemo {
  def main(args: Array[String]) = {

    val stat19 = ComplexStatCount.ofValues(1, 9)

    println(
      s"(1,9) = ${stat19}")

    val stat15 = ComplexStatCount.ofValues(1, 5)

    println(
      s"(1,5) = ${stat15}")

    val stat1225 = ComplexStatCount.ofValues(1, 2) - 2 + 5
    println(s"(1,2,-2,5) = ${stat1225}")

    val stat59 = ComplexStatCount.ofValues(9) - 5
    println(s"(-5,9) = ${stat59}")

    val stat122559 = stat1225 + stat59
    println(s"(1,2,-2,5) + (-5,9) = ${stat122559}")

    val stat591225 = stat59 + stat1225
    println(s"(-5,9) + (1,2,-2,5) = ${stat591225}")
    println(s"(1,5,9) - (1,5) = ${ComplexStatCount.ofValues(1,5,9) - ComplexStatCount.ofValues(1,5)}")
    println(s"(9) = ${ComplexStatCount.ofValues(9)}")
  }
}
