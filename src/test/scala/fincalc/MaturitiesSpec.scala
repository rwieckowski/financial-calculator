package fincalc

class MaturitiesSpec extends UnitSpec {
  def testMaturities(nofPeriods: Int, start: String, monthsToAdd: Int): List[String] =
    maturities(nofPeriods, date(start), monthsToAdd).map(_.toString)

  "maturities" should "return sequence with start date when number of periods is below 1" in {
    testMaturities(0, "2014-01-01", 1) should equal (List("2014-01-01"))
  }

  it should "return start date as first date" in {
    testMaturities(1, "2014-01-01", 1).head should equal("2014-01-01")
  }

  it should "return dates with defined intervals" in {
    testMaturities(3, "2014-01-01", 2) should
      equal (List("2014-01-01", "2014-03-01", "2014-05-01", "2014-07-01"))
  }

  it should "return last day of the month if month is shorter" in {
    testMaturities(3, "2014-01-31", 1) should
      equal (List("2014-01-31", "2014-02-28", "2014-03-31", "2014-04-30"))
  }
}
