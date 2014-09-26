import java.time.LocalDate

object Main extends App {
  import fincalc._

  val capital = Money(30000)
  val periods = 36
  val start = LocalDate.now()
  val rate = 0.1

  val customs = Map(2 -> Money(750.00), 4 -> Money(750.00), 6 -> Money(750.00))

  val p: Payments = Payments.equal(periods) //_ andThen Payments.custom(customs)
  val sch = findSchedule(capital, rate, start, p)

  println(s"capital.: $capital")
  println(s"start...: $start")
  println(s"months..: $periods")
  //println(s"amount..: $amount")
  println("interest: " + rate * 100.0 + "%")

  def fmt(inst: Payment): String = inst match {
    case Payment(d, a, p, i, b) =>
      "%s:   amount: %9s   principal: %9s   interest: %9s   balance: %9s".format(d, a, p, i, b)
  }

  sch.map(ps => ps.map(fmt).foreach(println))

  def benchmark() = {
      val s = System.currentTimeMillis()
      val n = 100000
      (1 to n).par.foreach(i => findAmount(capital, rate, start, p))
      val e = System.currentTimeMillis()
      val d = (e - s) / 1000.0
      "%d searches in %.3f sec -> %.4f per sec".format(n, d, n / d)
  }

  println(benchmark())
}
