package beklash
package alef

import util.*

import org.scalatest.funsuite.AnyFunSuite

class TestModel extends AnyFunSuite:

  def parse(s: String): Model =
    Alef.modelParser.run(s) match
      case Left(e)  => sys.error(s"$e")
      case Right(m) => m


  test("Model.interpret"):
    assertResult(expected = Right(0))(
      actual = parse("(- 10 (+ (* 3 2) (/ 8 2)))").interpret(
        Map.empty))

    assertResult(
      expected = Right(0))(
      actual = parse("(- 10 (+ (* $a 2) (/ 8 $b)))").interpret(
        Map("a" -> 3, "b" -> 2)))

    assertResult(
      expected = Left("unresolved variable: b"))(
      actual = parse("(- 10 (+ (* $a 2) (/ 8 $b)))").interpret(
        Map("a" -> 3)))
