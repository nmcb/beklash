package beklash
package alef

import org.scalatest.funsuite.AnyFunSuite

class TestAlef extends AnyFunSuite:

  import Alef.*
  import Model.*

  test("Alef.parse"):
    assertResult(Num(12))(parse("12"))
    assertResult(Num(12))(parse(" 12 "))
    assertResult(Num(-1))(parse("-1"))
    assertResult(Num(-1))(parse(" -1 "))
    assertResult(Var("a"))(parse("$a"))
    assertResult(Var("a"))(parse(" $a "))
    assertResult(Bin("+", Num(1), Num(2)))(parse("(+ 1 2)"))
    assertResult(Bin("+", Num(1), Num(2)))(parse(" ( + 1 2 ) "))

    assertResult(Bin("+", Bin("+", Var("a"), Num(2)), Bin("+", Num(2), Var("b"))))(parse("(+ (+ $a 2) (+ 2 $b))"))

  test("Alef.service.call"):
    val input = Map("a" -> 3, "b" -> 4)
    assertResult(30)(Alef.service.call("service-a.model")(input))
    assertResult(11)(Alef.service.call("service-b.model")(input))
