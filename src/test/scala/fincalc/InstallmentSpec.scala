package fincalc

import java.time.LocalDate

class InstallmentSpec extends UnitSpec {

  val interest = 750.00
  val calc = (a: Amount, start: LocalDate, end: LocalDate) => interest
  val capital = 10000.00
  val start = LocalDate.of(2014, 1, 1)
  val end = LocalDate.of(2014, 2, 1)
  val amount = 2000.00

  val installment = Installment.factory(calc)

  "installment" should "set date" in {
    installment(amount, capital, start, end).date should equal(end)
  }

  it should "set amount" in {
    installment(amount, capital, start, end).amount should equal(amount)
  }

  it should "set capital" in {
    installment(amount, capital, start, end).capital should equal(capital)
  }

  it should "calculate interest" in {
    installment(amount, capital, start, end).interest should equal(interest)
  }

  it should "calculate interest when it exceeds amount" in {
    installment(500.00, capital, start, end).interest should equal(500.00)
  }

  it should "calculate remaining capital" in {
    installment(amount, capital, start, end).remainingCapital should equal(8750.00)
  }
}
