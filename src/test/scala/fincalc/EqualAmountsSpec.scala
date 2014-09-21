package fincalc

class EqualAmountsSpec extends UnitSpec {
  "equalAmounts" should "return empty sequence when number of amounts is below 1" in {
    equalAmounts(0, 1000.00) should be ('empty)
  }

  it should "return sequence of amounts" in {
    equalAmounts(3, 1000.00) should equal(List(1000.00, 1000.00, 1000.00))
  }
}
