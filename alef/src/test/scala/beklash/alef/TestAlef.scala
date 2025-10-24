package beklash
package alef

import org.scalatest.funsuite.AnyFunSuite

class TestAlef extends AnyFunSuite:

  import Alef.*
  import Model.*

  test("Alef.parse"):
    assertResult(Val(12))(parse("12"))
    assertResult(Val(12))(parse(" 12 "))
    assertResult(Val(-1))(parse("-1"))
    assertResult(Val(-1))(parse(" -1 "))
    assertResult(Var("a"))(parse("$a"))
    assertResult(Var("a"))(parse(" $a "))
    assertResult(Bin("+", Val(1), Val(2)))(parse("(+ 1 2)"))
    assertResult(Bin("+", Val(1), Val(2)))(parse(" ( + 1 2 ) "))

    assertResult(Bin("+", Bin("+", Var("a"), Val(2)), Bin("+", Val(2), Var("b"))))(parse("(+ (+ $a 2) (+ 2 $b))"))

  test("Alef.service.call"):
    val input = Map("a" -> 3, "b" -> 4)
    assertResult(Right(30))(Alef.service.call("service-a.model")(input))
    assertResult(Right(11))(Alef.service.call("service-b.model")(input))
