import java.time.LocalDate

object Main extends App {
  import fincalc._

  val capital = Money(30000)
  val periods = 36
  val start = LocalDate.now()
  val dates = maturities(periods, start)
  val rate = 0.1
  val intrst = interest(rate)_

  private val customs = Map(2 -> Money(750.00), 4 -> Money(750.00), 6 -> Money(750.00))
  val amounts = Amounts.custom(customs)_ compose Amounts.equal(periods)

  def benchmark() = {
    val s = System.currentTimeMillis()
    val n = 100000
    (1 to n).par.foreach(i => findAmount(intrst, amounts, start, capital))
    val e = System.currentTimeMillis()
    val d = (e - s) / 1000.0
    "%d searches in %.3f sec -> %.4f per sec".format(n, d, n / d)
  }

  val amount = findAmount(intrst, amounts, start, capital)
  println(s"capital.: $capital")
  println(s"start...: $start")
  println(s"months..: $periods")
  println(s"amount..: $amount")
  println("interest: " + rate * 100.0 + "%")

  def fmt(inst: Installment): String = inst match {
    case Installment(d, a, c, i, rc) =>
      "%s:   amount: %9s   capital: %9s   interest: %9s   remaining: %9s".format(d, a, c, i, rc)
  }
  amount
    .map(a => schedule(intrst, dates, amounts(a), capital).map(fmt).mkString("\n"))
    .foreach(println)

  //println(benchmark())
}
