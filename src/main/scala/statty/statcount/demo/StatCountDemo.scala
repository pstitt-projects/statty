package statty.statcount.demo

import statty.statcount.StatCount

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object StatCountDemo {
  def main(args: Array[String]) = {

    val stat19 = StatCount(1, 9)

    println(s"(${StatCount(9,2)-5}")
    println(s"(${StatCount(1,2)-5}")
    println(s"(${StatCount(1,2,3)-StatCount(3,5)}")
    println(s"(${StatCount(1,2,3,5)-StatCount(3,5)}")
    println(
      s"(1,9) = ${stat19}")

    val stat15 = StatCount(1, 5)

    println(
      s"(1,5) = ${stat15}")

    val stat1225 = StatCount(1, 2) - 2 + 5
    println(s"(1,2,-2,5) = ${stat1225}")

    val stat59 = StatCount(9) - 5
    println(s"(-5,9) = ${stat59}")

    val stat122559 = stat1225 + stat59
    println(s"(1,2,-2,5) + (-5,9) = ${stat122559}")

    val stat591225 = stat59 + stat1225
    println(s"(-5,9) + (1,2,-2,5) = ${stat591225}")
    println(s"(1,5,9) - (1,5) = ${StatCount(1,5,9) - StatCount(1,5)}")
    println(s"(9) = ${StatCount(9)}")
  }
}
