package fincalc

class InterestSpec extends UnitSpec {
  def testInterest(rate: Double, capital: Double, days: Int) = interest(rate)(Money(capital), days).value

  "interest" should "calculate interest for year" in {
    testInterest(0.1, 1000.0, 365) should equal(100.0)
  }

  it should "calculate interest for month" in {
    testInterest(0.1, 1000.0, 31) should equal(8.49)
  }
}
