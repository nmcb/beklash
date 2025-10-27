package beklash.util

import org.scalatest.funsuite.AnyFunSuite

import scala.io.*
import scala.util.*

class TestJsonPath extends AnyFunSuite:

  import JsonPath.*

  test("JsonPath.jsonPathParser"):
    import JPTest.*
    val parser = JsonPath.jsonPathParser
    assertResult(Right(Root(Step(JPTypeTest("object"), None, JPLocation.Relative, Nil))))(actual = parser.run("$.object()"))

  test("JsonPath.nodeTest"):
    import JPTest.*
    val parser = JsonPath.nodeTest
    assertResult(Right(JPTypeTest("object")))(actual = parser.run("object()"))
    assertResult(Right(JPTypeTest("array")))(actual = parser.run("array()"))
    assertResult(Right(JPTypeTest("string")))(actual = parser.run("string()"))
    assertResult(Right(JPTypeTest("number")))(actual = parser.run("number()"))
    assertResult(Right(JPTypeTest("boolean")))(actual = parser.run("boolean()"))
    assertResult(Right(JPTypeTest("null")))(actual = parser.run("null()"))
    assertResult(Right(JPNameTest("*")))(actual = parser.run("*"))
    assertResult(Right(JPNameTest("abcdef")))(actual = parser.run("'abcdef'"))


