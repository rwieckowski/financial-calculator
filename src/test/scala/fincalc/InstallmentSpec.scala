package fincalc

class InstallmentSpec extends UnitSpec {

  def testInstallment(interest: Double = 0.0, amount: Double = 0.0, capital: Double = 0.0,
                      start: String = "1900-01-01", end: String = "1900-01-01") = {
    Installment((m: Money, d: Int) => Money(interest), date(start), date(end), Money(amount), Money(capital))
  }

  "installment" should "set date" in {
    testInstallment(end = "2014-01-01").date should equal(date("2014-01-01"))
  }

  it should "set amount" in {
    testInstallment(amount = 1000.00).amount.value should equal(1000.00)
  }

  it should "set capital" in {
    testInstallment(capital = 10000.00).capital.value should equal(10000.00)
  }

  it should "calculate interest" in {
    testInstallment(interest = 750.00, amount = 2000.00).interest.value should equal(750.00)
  }

  it should "calculate interest when it exceeds amount" in {
    testInstallment(interest = 750.00, amount = 500.00).interest.value should equal(500.00)
  }

  it should "calculate remaining capital" in {
    testInstallment(interest = 750.00, amount = 2000.00, capital = 10000.00).remainingCapital.value should equal(8750.00)
  }
}
