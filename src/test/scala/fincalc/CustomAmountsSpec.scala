package fincalc

class CustomAmountsSpec extends UnitSpec {
  "customAmounts" should "return empty sequence when no amounts defined" in {
    customAmounts(Map({ 0 -> 500.00}), Nil) should be ('empty)
  }

  it should "return unchanged list of amounts when no custom amounts defined" in {
    customAmounts(Map.empty, List(1000.00)) should equal (List(1000.00))
  }

  it should "change amounts based on index" in {
    customAmounts(Map({ 1 -> 500.00}), List(1000.00, 2000.00, 4000.00)) should
      equal(List(1000.00, 500.00, 4000.00))
  }
}
