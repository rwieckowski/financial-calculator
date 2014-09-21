package fincalc

import java.time.LocalDate

class ScheduleSpec extends UnitSpec {
  val installment = Installment(null, Double.NaN, Double.NaN, Double.NaN, Double.NaN)
  val factory = (a: Amount, c: Amount, s: LocalDate, e: LocalDate) => installment
  val noPeriods = List(LocalDate.of(2014, 1, 1))
  val onePeriod = List(LocalDate.of(2014, 1, 1), LocalDate.of(2014, 2, 1))
  val threePeriods = List(
    LocalDate.of(2014, 1, 1), LocalDate.of(2015, 1, 1), LocalDate.of(2016, 1, 1), LocalDate.of(2014, 4, 1))
  val capital = 10000.00
  val amounts = List.fill(3)(2000.00)

  "schedule" should "return empty sequence for 0 periods" in {
    schedule(factory, noPeriods, amounts, 0.0) should be('empty)
  }

  it should "return one installment for 1 period" in {
    schedule(factory, onePeriod, amounts, 0.0) should have size 1
  }

  it should "return one installments for many periods" in {
    schedule(factory, threePeriods, amounts, 0.0) should have size (threePeriods.size - 1)
  }
}
