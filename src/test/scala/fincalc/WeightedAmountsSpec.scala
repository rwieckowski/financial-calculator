package fincalc

class WeightedAmountsSpec extends UnitSpec {
  "weightedAmount" should "return empty sequence when no weights defined" in {
    weightedAmounts(Nil, 1000.00) should be ('empty)
  }

  it should "return amounts for weights" in {
    weightedAmounts(List(1.0, 2.0, 4.0), 1000.00) should
      equal(List(1000.00, 2000.00, 4000.00))
  }
}
