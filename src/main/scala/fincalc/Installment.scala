package fincalc

import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class Installment(date: LocalDate, amount: Money, capital: Money, interest: Money, remainingCapital: Money)

object Installment {
  def apply(interest: Interest, start: LocalDate, end: LocalDate, amount: Money, capital: Money): Installment = {
    def daysBetween = ChronoUnit.DAYS.between(start, end).toInt

    val i = interest(capital, daysBetween).min(amount)
    val rc = capital + i - amount
    Installment(end, amount, capital, i, rc)
  }
}
