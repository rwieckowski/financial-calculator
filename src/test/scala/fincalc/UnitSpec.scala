package fincalc

import java.time.LocalDate

import org.scalatest.{ShouldMatchers, FlatSpec}

abstract class UnitSpec extends FlatSpec with ShouldMatchers {
  def date(s: String): LocalDate = LocalDate.parse(s)
}