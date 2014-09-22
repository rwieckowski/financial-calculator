package fincalc

class AmountsSpec extends UnitSpec {
  def testCustom(customs: Map[Int, Double], amounts: List[Double]): List[Double] = {
    val cm = customs.toList.map { case (i, m) => (i, Money(m))}.toMap
    Amounts.custom(cm)(amounts.map(Money(_))).map(_.value)
  }

  "custom" should "return empty sequence when no amounts defined" in {
    testCustom(Map(0 -> 500.00), Nil) should be('empty)
  }

  it should "return unchanged list of amounts when no custom amounts defined" in {
    testCustom(Map.empty, List(1000.00)) should equal(List(1000.00))
  }

  it should "change amounts based on index" in {
    testCustom(Map(1 -> 500.00), List(1000.00, 2000.00, 4000.00)) should
      equal(List(1000.00, 500.00, 4000.00))
  }

  def testEqual(nofAmounts: Int, amount: Double): List[Double] =
    Amounts.equal(nofAmounts)(Money(amount)).map(_.value)

  "equal" should "return empty sequence when number of amounts is below 1" in {
    testEqual(0, 1000.00) should be ('empty)
  }

  it should "return sequence of amounts" in {
    testEqual(3, 1000.00) should equal(List(1000.00, 1000.00, 1000.00))
  }

  def testWeighted(weights: List[Double], amount: Double): List[Double] =
    Amounts.weighted(weights)(Money(amount)).map(_.value)

  "weighted" should "return empty sequence when no weights defined" in {
    testWeighted(Nil, 1000.00) should be ('empty)
  }

  it should "return amounts for weights" in {
    testWeighted(List(1.0, 2.0, 4.0), 1000.00) should
      equal(List(1000.00, 2000.00, 4000.00))
  }
}
