package fincalc

class Money private (val value: Double) {

  def + (that: Money): Money = Money(value + that.value)

  def - (that: Money): Money = Money(value - that.value)

  def * (that: Double): Money = Money(value * that)

  def / (that: Double): Money = Money(value / that)

  def min(that: Money) = if (value < that.value) this else that

  override def toString = "$%.2f".format(value)

}

object Money {
  def apply(value: Double): Money = new Money(r(value))

  private def r(v: Double): Double = Math.round(v * 100.0) / 100.0
}