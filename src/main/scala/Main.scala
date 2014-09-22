import java.time.LocalDate

object Main extends App {
  import fincalc._

  val capital = 30000
  val duration = 36
  val start = LocalDate.now()
  val dates = maturities(duration, start)
  val rate = 10.0
  val factory = Installment.factory(interest(rate percent))

  private val customs = Map(2 -> Money(750.00), 4 -> Money(750.00), 6 -> Money(750.00))
  val amounts = Amounts.custom(customs)_ compose Amounts.equal(duration)

  def benchmark() = {
    val start = System.currentTimeMillis()
    val n = 100000
    (1 to n).par.foreach(i => findAmount(factory, dates, amounts, capital))
    val end = System.currentTimeMillis()
    val duration = (end - start) / 1000.0
   "%d searches in %.3f sec -> %.4f per sec".format(n, duration, n / duration)
  }

  val amount = findAmount(factory, dates, amounts, capital).get.$
  println(s"capital.: $capital")
  println(s"start...: $start")
  println(s"months..: $duration")
  println(s"amount..: $amount")
  println(s"interest: $rate%")

  def fmt(i: Installment) = {
    "%s:\tamount: %7.2f\tcapital: %7.2f\tinterest: %7.2f\tremaining: %7.2f"
      .format(i.date, i.amount, i.capital, i.interest, i.remainingCapital)
  }
  schedule(factory, dates, amounts(Money(amount)).map(_.value), capital).map(fmt).foreach(println)

  println(benchmark())
}
