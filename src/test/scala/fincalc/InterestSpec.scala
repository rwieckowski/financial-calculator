package fincalc

class InterestSpec extends UnitSpec {
  "interest" should "calculate interest for year" in {
    Interest(10.0 percent)(1000.0, "2014-01-01" toDate, "2015-01-01" toDate) should equal(100.0)
  }

  it should "calculate interest for month" in {
    Interest(10.0 percent)(1000.0, "2014-01-01" toDate, "2014-02-01" toDate) should equal(8.49)
  }
}
