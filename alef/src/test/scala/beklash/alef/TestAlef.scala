package beklash
package alef

import util.*

import org.scalatest.funsuite.AnyFunSuite

class TestAlef extends AnyFunSuite:

  import Model.*

  test("Alef.parse"):
    val parser = Alef.modelParser
    assertResult(
      expected = Right(Bin("+", Val(1), Val(2))))(
      actual = parser.run("(+ 1 2)")
    )
    assertResult(
      expected = Right(Bin("-", Val(1), Val(2))))(
      actual = parser.run("(- 1 2)")
    )
    assertResult(
      expected = Right(Bin("*", Val(1), Val(2))))(
      actual = parser.run("(* 1 2)")
    )
    assertResult(
      expected = Right(Bin("/", Val(1), Val(2))))(
      actual = parser.run("(/ 1 2)")
    )
    assertResult(
      expected = Right(Bin("+", Val(1), Val(2))))(
      actual = parser.run(" ( + 1 2 ) ")
    )
    assertResult(
      expected = Right(Bin("+", Var("a"), Var("b"))))(
      actual = parser.run("(+ $a $b)")
    )
    assertResult(
      expected = Right(Bin("+", Var("a"), Var("b"))))(
      actual = parser.run(" ( + $a $b ) ")
    )
    assertResult(
      expected = Right(Bin("+", Bin("+", Var("a"), Val(2)), Bin("+", Val(2), Var("b")))))(
      actual = parser.run("(+ (+ $a 2) (+ 2 $b))")
    )
    assertResult(
      expected = Right(Bin("+", Val(10), Bin("-", Val(3), Val(2)))))(
      actual = parser.run("(+ 10 (- 3 2))")
    )
    assertResult(
      expected = Right(Bin("+", Bin("-", Val(3), Val(2)), Val(10))))(
      actual = parser.run("(+ (- 3 2) 10)")
    )

  test("Alef.service.call"):
    val input = Map("a" -> 3.0, "b" -> 4.0)
    assertResult(Right(30))(Alef.service.call("service-a.model")(input))
    assertResult(Right(11))(Alef.service.call("service-b.model")(input))
