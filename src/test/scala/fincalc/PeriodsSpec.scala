package fincalc

class PeriodsSpec extends UnitSpec {
  "periods" should "return sequence with start date when number of periods is below 1" in {
    periods(0, "2014-01-01" toDate) should equal (List("2014-01-01" toDate))
  }

  it should "return sequence with start date when months to add is below 1" in {
    periods(1, "2014-01-01" toDate, 0) should equal (List("2014-01-01" toDate))
  }

  it should "return start date as first date" in {
    periods(1, "2014-01-01" toDate).head should equal("2014-01-01" toDate)
  }

  it should "return consecutive dates with defined intervals" in {
    periods(3, "2014-01-01" toDate, 2) should
      equal (List("2014-01-01", "2014-03-01", "2014-05-01", "2014-07-01").map(_.toDate))
  }
}
