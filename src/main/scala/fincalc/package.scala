import java.time.LocalDate
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec
import scala.collection.SeqView
import scala.runtime.ZippedTraversable3

package object fincalc {
  type Payments = ((LocalDate, Money)) => List[(LocalDate, Money)]

  object Payments {
    def equal(nofPayments: Int)(p: (LocalDate, Money)): List[(LocalDate, Money)] = {
      val (start, amount) = p
      (1 to nofPayments)
        .map( i => (start.plusMonths(i), amount))
        .toList
    }

    def weighted(weights: List[Double])(p: (LocalDate, Money)): List[(LocalDate, Money)] = {
      val (start, amount) = p
      weights
        .zipWithIndex
        .map { case (w, i) => (start.plusMonths(i + 1), amount * w) }
    }

    def custom(customs: Map[Int, Money])(payments: List[(LocalDate, Money)]): List[(LocalDate, Money)] = {
      payments
        .zipWithIndex
        .map { case ((d, m), i) => (d, customs.getOrElse(i, m)) }
    }
  }

  case class Payment(date: LocalDate, amount: Money, principal: Money, interest: Money, balance: Money)

  object Payment {
    def apply(start: LocalDate, end: LocalDate, amount: Money, principal: Money, interestRate: Double): Payment = {
      val days = ChronoUnit.DAYS.between(start, end)
      val interest = amount.min(principal * interestRate * days / 365.0)
      val balance = principal + interest - amount
      Payment(end, amount, principal, interest, balance)
    }
  }

  def bisect[T](a: T, b: T, tol: T)(f: T => T)(implicit numeric: Fractional[T]): Option[T] = {
    def sameSign(x: T, y: T) = numeric.signum(x) == numeric.signum(y)
    def midpoint(x: T, y: T) = numeric.div(numeric.plus(x, y), numeric.fromInt(2))
    def inTolerance(x: T, y: T) = numeric.lteq(numeric.minus(y, x), tol)
    @tailrec
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
    if (sameSign(fa, fb)) None else Some(loop(a, b, fa, fb))
  }

  def findAmount(principal: Money, interestRate: Double, start: LocalDate, payments: Payments): Option[Money] = {
    def balance(rev: List[Payment]): Double = rev.head.balance.value

    bisect[Double](0.00, principal.value, 0.005) {
      a => balance(reversedSchedule(principal, interestRate, start, payments(start, Money(a))))
    } map Money.apply
  }

  def schedule(principal: Money, interestRate: Double, start: LocalDate, amount: Money,
               payments: Payments): List[Payment] = {
    reversedSchedule(principal, interestRate, start, payments(start, amount)).reverse
  }

  def reversedSchedule(principal: Money, interestRate: Double, start: LocalDate,
                               payments: List[(LocalDate, Money)]): List[Payment] = {
    @tailrec
    def loop(pr: Money, s: LocalDate, ps: List[(LocalDate, Money)], accu: List[Payment]): List[Payment] = ps match {
      case (e, a) :: t =>
        val p = Payment(s, e, a, pr, interestRate)
        loop(p.balance, e, t, p :: accu)
      case _ => accu
    }

    loop(principal, start, payments, Nil)
  }

  def findSchedule(principal: Money, interestRate: Double, start: LocalDate,
                   payments: Payments): Option[List[Payment]] = {
    findAmount(principal, interestRate, start, payments)
      .map(a => schedule(principal, interestRate, start, a, payments))
  }
}