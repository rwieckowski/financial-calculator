package fincalc

import org.joda.time.LocalDate
import org.scalatest.{FlatSpec, ShouldMatchers}

abstract class UnitSpec extends FlatSpec with ShouldMatchers {
  def date(s: String): LocalDate = LocalDate.parse(s)
}