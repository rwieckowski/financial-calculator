import java.time.LocalDate
import java.time.temporal.ChronoUnit

package object fincalc {

  type Amount = Double
  type Percent = Double
  type InterestCalc = (Amount, LocalDate, LocalDate) => Amount

  implicit class AmountOps(a: Amount) {
    def $ = (a * 100.0).round / 100.0
  }

  implicit class PercentOps(p: Percent) {
    def percent = (p * 100.0).round / 10000.0
  }

  implicit class StringOps(s: String) {
    def toDate = LocalDate.parse(s)
  }


  type Interest = (Amount, LocalDate, LocalDate) => Amount

  private def interest(rate: Percent)(capital: Amount, start: LocalDate, end: LocalDate): Amount =
      (rate * capital * ChronoUnit.DAYS.between(start, end) / 365.0).$

  object Interest {
    def apply(rate: Percent): Interest = interest(rate)
  }


  def periods(nofPeriods: Int, start: LocalDate, monthsToAdd: Int = 1): List[LocalDate] = {
    if (nofPeriods < 1 || monthsToAdd < 1) List(start)
    else start :: periods(nofPeriods - 1, start.plusMonths(monthsToAdd), monthsToAdd)
  }


  type AmountFactory = Amount => List[Amount]

  def equalAmounts(nofAmounts: Int, amount: Amount): List[Amount] = {
    List.fill(nofAmounts)(amount)
  }

  def weightedAmounts(weights: List[Double], amount: Amount): List[Amount] = {
    weights.map { amount * _ }
  }

  def customAmounts(custom: Map[Int, Amount], amounts: List[Amount]): List[Amount] = {
    amounts.zipWithIndex.map { case (a, i) => custom.getOrElse(i, a) }
  }

  object Amounts {
    def equal(nofAmounts: Int): AmountFactory = {
      equalAmounts(nofAmounts, _)
    }

    def weighted(weights: List[Double]): AmountFactory = {
        weightedAmounts(weights, _)
    }

    def custom(custom: Map[Int, Amount], amounts: AmountFactory): AmountFactory = {
      if (custom.isEmpty)
        amounts
      else
        (a: Amount) => customAmounts(custom, amounts(a))
    }
  }

  case class Installment(date: LocalDate, amount: Amount, capital: Amount, interest: Amount, remainingCapital: Amount)

  def installment(interest: Interest, amount: Amount, capital: Amount, start: LocalDate, end: LocalDate): Installment = {
    val i = interest(capital, start, end).min(amount)
    val rc = capital + i - amount
    Installment(date = end, amount = amount, capital = capital, interest = i.$, remainingCapital = rc.$)
  }

  type InstalmentFactory = (Amount, Amount, LocalDate, LocalDate) => Installment

  object Installment {
    def factory(i: Interest): InstalmentFactory = installment(i, _, _, _, _)
  }


  def schedule(installment: InstalmentFactory, dates: List[LocalDate], amounts: List[Amount], capital: Amount): List[Installment] = {
    dates match {
      case start :: end :: rest => {
        val i = installment(amounts.head, capital, start, end)
        i :: schedule(installment, end :: rest, amounts.tail, i.remainingCapital)
      }
      case _ => Nil
    }
  }

  object Schedule {
    def apply(interest: Interest, capital: Amount, dates: List[LocalDate], amounts: List[Amount]): List[Installment] = {
      null
    }
  }

  def bisect[T](a: T, b: T, tol: T)(f: T => T)(implicit numeric: Fractional[T]): Option[T] = {
    def sameSign(x: T, y: T) = numeric.signum(x) == numeric.signum(y)
    def midpoint(x: T, y: T) = numeric.div(numeric.plus(x, y), numeric.fromInt(2))
    def inTolerance(x: T, y: T) = numeric.lteq(numeric.minus(y, x), tol)

    val fa = f(a)
    val fb = f(b)
    if (sameSign(fa, fb)) None
    else {
      def loop(a: T, b: T, fa: T, fb: T): T = {
        val m = midpoint(a, b)
        if (inTolerance(a, b)) m
        else {
          val fm = f(m)
          if (sameSign(fm, fb)) loop(a, m, fa, fm)
          else loop(m, b, fm, fb)
        }
      }
      Some(loop(a, b, fa, fb))
    }
  }

  def findAmount(installment: InstalmentFactory, dates: List[LocalDate], amounts: AmountFactory, capital: Amount) = {
    bisect[Amount](0.00, capital, 0.01) {
      a => schedule(installment, dates, amounts(a), capital).last.remainingCapital
    }
  }
}