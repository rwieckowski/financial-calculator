package fincalc

import scala.collection.mutable.ListBuffer

class BisectSpec extends UnitSpec {

  def const(vs: Double*)(c: Double): (Double => Double) = {
    var n = -1
    def f(x: Double) = {
      n += 1
      if (n < vs.size) vs(n) else c
    }
    f
  }
  def values(vs: Double*): (Double => Double) = {
    var n = -1
    def f(x: Double) = {
      n += 1
      vs(n)
    }
    f
  }

  class Spy {
    private val as = ListBuffer[Double]()

    def apply(f: Double => Double): Double => Double = {
      def g(x: Double) = {
        as += x
        f(x)
      }
      g
    }

    def args(): List[Double] = as.toList
  }

  "bisection" should "return None if function values for both ends have the same sign" in {
    bisect(a = -1.0, b = 1.0, tol = 1.0)(values(1.0, 1.0)) should be(None)

    bisect(a = -1.0, b = 1.0, tol = 1.0)(values(-1.0, -1.0)) should be(None)
  }

  it should "apply function for both ends" in {
    val spy = new Spy()
    bisect(a = -1.0, b = 1.0, tol = 1.0)(spy(const(-1.0, 1.0)(0.0)))

    spy.args().take(2) should equal(List(-1.0, 1.0))
  }

  it should "apply function to midpoint if range is greater than tolerance" in {
    val spy = new Spy()
    bisect(a = -1.0, b = 1.0, tol = 1.0)(spy(values(-1.0, 1.0, 1.0)))

    spy.args() should equal(List(-1.0, 1.0, 0.0))
  }

  it should "return midpoint if range is lower or equal than tolerance" in {
    bisect(a = -1.0, b = 1.0, tol = 2.0)(values(-1.0, 1.0)) should equal(Some(0.0))
  }

  it should "take left half if function values for midpoint and right end have the same sign" in {
    bisect(a = -1.0, b = 1.0, tol = 1.0)(values(-1.0, 1.0, 1.0)) should equal(Some(-0.5))
  }

  it should "take right half if function values for midpoint and left end have the same sign" in {
    bisect(a = -1.0, b = 1.0, tol = 1.0)(values(-1.0, 1.0, -1.0)) should equal(Some(0.5))
  }

  it should "return function root" in {
    bisect(2.0, 4.0, 0.0001) { x => Math.sin(x) } getOrElse Double.NaN should equal(3.1415 +- 0.0001)
  }
}
