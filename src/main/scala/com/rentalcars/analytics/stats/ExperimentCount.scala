package com.rentalcars.analytics.stats

/**
  * Created by Paul Stitt on 01/05/2017.
  */
case class ExperimentCount(impressions: Int, goalCount: ComplexStatCount) {
  def mergeImpression(): ExperimentCount =
    ExperimentCount(impressions + 1, goalCount)

  def mergeBinaryGoal(): ExperimentCount =
    ExperimentCount(impressions, goalCount + 0)

  def mergeValueGoal(value: BigDecimal): ExperimentCount =
    ExperimentCount(impressions, goalCount + value)

  def unmergeBinaryGoal(): ExperimentCount =
    ExperimentCount(impressions, goalCount - 0)

  def unmergeValueGoal(value: BigDecimal): ExperimentCount =
    ExperimentCount(impressions, goalCount - value)

  def +(other: ExperimentCount): ExperimentCount = {
    ExperimentCount(impressions + other.impressions,
      goalCount + other.goalCount)
  }

  def +(value: BigDecimal): ExperimentCount = {
    ExperimentCount(impressions, goalCount + value)
  }

  def -(value: BigDecimal): ExperimentCount = {
    ExperimentCount(impressions, goalCount - value)
  }

  override def toString(): String = {
    s"ExperimentCount[impressions=${impressions}, goals=${goalCount}"
  }
}
