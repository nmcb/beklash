package beklash
package util

import util.*

import org.scalatest.funsuite.AnyFunSuite

class TestJson extends AnyFunSuite:

  import Json.*

  test("Json.jsonParser"):
    val parser = Json.jsonParser
    assertResult(
      expected =
        Right(
          JObject(
            Map(
              "a" -> JString("b"),
              "c" -> JNull,
              "d" -> JBool(true),
              "e" -> JBool(false),
              "f" -> JArray(
                IndexedSeq(
                  JNumber(1),
                  JNumber(2),
                  JNumber(3)
                )
              )
            )
          )
        )
      )(
      actual = parser.run("""{"a":"b","c":null,"d":true,"e":false,"f":[1,2,3]}""")
    )
