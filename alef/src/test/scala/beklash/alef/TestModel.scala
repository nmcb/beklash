package beklash.alef

import org.scalatest.funsuite.AnyFunSuite

class TestModel extends AnyFunSuite:

  import Alef.*

  test("Model.interpret"):
    assertResult(expected = 0)(
      actual = parse("(- 10 (+ (* 3 2) (/ 8 2)))").interpret(
        Map.empty))

    assertResult(
      expected = 0)(
      actual = parse("(- 10 (+ (* $a 2) (/ 8 $b)))").interpret(
        Map("a" -> 3, "b" -> 2)))
