package fincalc

class Money private (val value: Double) {
  def * (that: Double): Money = Money(value * that)

  def *: (that: Double): Money = Money(value * that)

  def / (that: Double): Money = Money(value / that)
}

object Money {
  def apply(value: Double): Money = new Money(r(value))

  private def r(v: Double): Double = Math.round(v * 100.0) / 100.0
}