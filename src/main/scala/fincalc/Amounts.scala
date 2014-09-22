package fincalc

object Amounts {
  def equal(nofAmounts: Int)(amount: Money): List[Money] = List.fill(nofAmounts)(amount)

  def weighted(weights: List[Double])(amount: Money): List[Money] = {
    weights.map { amount * _ }
  }

/*
  def custom(custom: Map[Int, Amount])(amounts: Amounts): Amounts = {
    if (custom.isEmpty)
      amounts
    else
      amounts.zipWithIndex.map { case (a, i) => custom.getOrElse(i, a) }
  }
*/

  def custom(customs: Map[Int, Money])(amounts: List[Money]): List[Money] =
    amounts.zipWithIndex.map { case (a, i) => customs.getOrElse(i, a) }
}
