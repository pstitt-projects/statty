package statty.statcount

import ComplexStatImplicits._

/**
  * Created by Paul Stitt on 01/05/2017.
  */
object ComplexStatImplicits {
  implicit def bdToString(value: BigDecimal): String = {
    s"${value.setScale(2, BigDecimal.RoundingMode.HALF_UP)}"
  }
}

object ComplexStatCount {
  def apply(n: Int,
            mean: BigDecimal,
            M2: BigDecimal): ComplexStatCount = {
    new CalculableComplexStatCount(n, mean, M2)
  }

  def ofValues(valuesToAdd: BigDecimal*): ComplexStatCount = {
    var stat = ComplexStatCount(0, 0, 0)
    valuesToAdd.foreach(value => {
      stat = stat + value
    })
    stat
  }

  def ofAntiValues(valuesToAdd: BigDecimal*): ComplexStatCount = {
    var stat = ComplexStatCount(0, 0, 0)
    valuesToAdd.foreach(value => {
      stat = stat - value
    })
    stat
  }
}

trait ComplexStatCount {
  def count: Int

  def n: Int = count

  def mean: BigDecimal

  def M2: BigDecimal

  def +(value: BigDecimal): ComplexStatCount

  def +(other: ComplexStatCount): ComplexStatCount

  def -(value: BigDecimal): ComplexStatCount

  def -(other: ComplexStatCount): ComplexStatCount

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


