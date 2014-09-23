import java.time.LocalDate

import scala.annotation.tailrec

package object fincalc {
  def maturities(nofPeriods: Int, start: LocalDate, monthsToAdd: Int = 1): List[LocalDate] = {
    (0 to nofPeriods).map(i => start.plusMonths(i * monthsToAdd)).toList
  }

  type Interest = (Money, Int) => Money

  def interest(rate: Double)(capital: Money, days: Int): Money =
    capital * rate * days / 365.0

  type Amounts = Money => List[Money]

  def schedule(interest: Interest, dates: List[LocalDate], amounts: List[Money], capital: Money): List[Installment] = {
    @tailrec
    def loop(payments: List[(LocalDate, LocalDate, Money)],
             capital: Money, is: List[Installment]): List[Installment] = payments match {
      case (start, end, amount) :: ps => {
        val i = Installment(interest, start, end, amount, capital)
        loop(ps, i.remainingCapital, i :: is)
      }
      case Nil => is
    }

    val payments = (dates, dates.tail, amounts).zipped.toList
    loop(payments, capital, Nil).reverse
  }

  def bisect[T](a: T, b: T, tol: T)(f: T => T)(implicit numeric: Fractional[T]): Option[T] = {
    def sameSign(x: T, y: T) = numeric.signum(x) == numeric.signum(y)
    def midpoint(x: T, y: T) = numeric.div(numeric.plus(x, y), numeric.fromInt(2))
    def inTolerance(x: T, y: T) = numeric.lteq(numeric.minus(y, x), tol)
    def loop(a: T, b: T, fa: T, fb: T): T = {
      val m = midpoint(a, b)
      if (inTolerance(a, b)) m
      else {
        val fm = f(m)
        if (sameSign(fm, fb)) loop(a, m, fa, fm)
        else loop(m, b, fm, fb)
      }
    }

    val fa = f(a)
    val fb = f(b)
    if (sameSign(fa, fb)) None
    else Some(loop(a, b, fa, fb))
  }

  def findAmount(interest: Interest, amounts: Amounts, start: LocalDate, capital: Money): Option[Money] = {
    val nofPeriods = amounts(Money(0.0)).size
    val dates = maturities(nofPeriods, start)
    bisect[Double](0.00, capital.value, 0.01) {
      a => remainingCapital(schedule(interest, dates, amounts(Money(a)), capital)).value
    } map Money.apply
  }

  def remainingCapital(is: List[Installment]): Money = is.last.remainingCapital
}
