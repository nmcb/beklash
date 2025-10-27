package beklash
package util

import util.*
import org.scalatest.funsuite.AnyFunSuite

import scala.io.*

class TestJson extends AnyFunSuite:

  import Json.*

  val stringJson: String =
    Source.
      fromResource("test-structure.json")
      .mkString

  val expectedJson: Json =
    JObject(
      Map(
        "a" -> JString("b"),
        "c" -> JNull,
        "d" -> JBool(true),
        "e" -> JBool(false),
        "f" -> JArray(
          Vector(
            JNumber(1.0),
            JNumber(2.0),
            JNumber(3.0))
        ),
        "g" -> JObject(
          Map(
            "ga" -> JString("bb"),
            "gc" -> JNull,
            "gd" -> JBool(true),
            "ge" -> JBool(false),
            "gg" -> JObject(
              Map(
                "gga" -> JString("bbb")
              )
            ),
            "gf" -> JArray(
              Vector(
                JNumber(1.0),
                JNumber(2.0),
                JNumber(3.0)
              )
            )
          )
        )
      )
    )

  test("Json.jsonParser"):
    val parser = Json.jsonParser
    assertResult(Right(expectedJson))(actual = parser.run(stringJson))
