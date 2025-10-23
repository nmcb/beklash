package beklash.alef

import org.scalatest.funsuite.AnyFunSuite

class TestModel extends AnyFunSuite:

  import Alef.*
  import Model.*

  test("Alef.parse"):
    assertResult( Num(12))(parse("12"))
    assertResult( Num(12))(parse(" 12 "))
    assertResult( Num(-1))(parse("-1"))
    assertResult( Num(-1))(parse(" -1 "))
    assertResult( Bin("+", Num(1), Num(2)))(parse("(+ 1 2)"))
    assertResult( Bin("+", Num(1), Num(2)))(parse(" ( + 1 2 ) "))
    assertResult( Bin("+", Bin("+", Num(1), Num(2)), Bin("+", Num(1), Num(2))))(parse("(+ (+ 1 2) (+ 1 2))"))

  test("Alef.interpret"):
    assertResult(expected = 10)(actual = parse("(+ (* 3 2) (/ 8 2))").interpret)

