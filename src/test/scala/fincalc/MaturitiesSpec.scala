package fincalc

class MaturitiesSpec extends UnitSpec {
  "maturities" should "return sequence with start date when number of periods is below 1" in {
    maturities(0, "2014-01-01" toDate, 1) should equal (List("2014-01-01" toDate))
  }

  it should "return start date as first date" in {
    maturities(1, "2014-01-01" toDate, 1).head should equal("2014-01-01" toDate)
  }

  it should "return dates with defined intervals" in {
    maturities(3, "2014-01-01" toDate, 2) should
      equal (List("2014-01-01", "2014-03-01", "2014-05-01", "2014-07-01").map(_.toDate))
  }

  it should "return last day of the month if month is shorter" in {
    maturities(3, "2014-01-31" toDate, 1) should
      equal (List("2014-01-31", "2014-02-28", "2014-03-31", "2014-04-30").map(_.toDate))
  }
}
