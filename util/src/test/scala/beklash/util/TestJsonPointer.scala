package beklash.util

import org.scalatest.funsuite.AnyFunSuite

import scala.io.*
import scala.util.*

class TestJsonPointer extends AnyFunSuite:

  import Json.*
  import JsonPointer.*
  import Segment.*

  val parser = jsonPointerParser

  test("JsonPointer.jsonPointerParser"):

    assertResult(
      Right(
        JsonPointer(Nil)
      )
    )(actual = parser.run("/"))

    assertResult(
      Right(
        JsonPointer(
          List(
            Name("books"),
            Name("2"),
            Name("price")
          )
        )
      )
    )(actual = parser.run("/books/2/price"))

  assertResult(
    Right(
      JsonPointer(
        List(
          Name("books"),
          Name("2"),
          Variable("price")
        )
      )
    )
  )(actual = parser.run("/books/2/{price}"))

  assertResult(
    Right(
      JsonPointer(
        List(
          Variable("books"),
          Name("2"),
          Name("price")
        )
      )
    )
  )(actual = parser.run("/{books}/2/price"))
