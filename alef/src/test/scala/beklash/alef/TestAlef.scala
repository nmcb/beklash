package beklash
package alef

import org.scalatest.funsuite.AnyFunSuite

class TestCalculator extends AnyFunSuite:


  import Calculator.*
  import AST.*

  test("Calculator.parse") {
    assertResult(expected = Num(12))(actual   = Calculator.parse("12"))
    assertResult(expected = Num(12))(actual   = Calculator.parse(" 12 "))

    assertResult(expected = Num(-1))(actual   = Calculator.parse("-1"))
    assertResult(expected = Num(-1))(actual   = Calculator.parse(" -1 "))

    assertResult(expected = Bin("+", Num(1), Num(2)))(actual = Calculator.parse("(+ 1 2)"))
    assertResult(expected = Bin("+", Num(1), Num(2)))(actual = Calculator.parse(" ( + 1 2 ) "))
  }