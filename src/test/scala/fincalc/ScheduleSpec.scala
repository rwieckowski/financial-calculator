package fincalc

import java.time.LocalDate

class ScheduleSpec extends UnitSpec {
  val noPeriods = List(LocalDate.of(2014, 1, 1))
  val onePeriod = List(LocalDate.of(2014, 1, 1), LocalDate.of(2014, 2, 1))
  val threePeriods = List(
    LocalDate.of(2014, 1, 1), LocalDate.of(2015, 1, 1), LocalDate.of(2016, 1, 1), LocalDate.of(2014, 4, 1))
  val capital = 10000.00
  val amounts = List.fill(3)(2000.00)

  def testSchedule(dates: List[String], amounts: List[Double], capital: Double): List[Installment] =
    schedule(interest(0.1), dates.map(date), amounts.map(Money(_)), Money(capital))

  "schedule" should "return empty sequence for 0 periods" in {
    testSchedule(List("2014-01-01"), Nil, 0.0) should be('empty)
  }

  it should "return one installment for 1 period" in {
    testSchedule(List("2014-01-01", "2014-02-01"), List(0.0), 0.0) should have size 1
  }

  it should "return one installments for many periods" in {
    testSchedule(List("2014-01-01", "2014-02-01", "2014-03-01", "2014-04-01"), List(0.0, 0.0, 0.0), 0.0) should
      have size (threePeriods.size - 1)
  }
}
