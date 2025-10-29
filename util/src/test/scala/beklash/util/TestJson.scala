package beklash
package util

import util.*
import org.scalatest.funsuite.AnyFunSuite

class TestJson extends AnyFunSuite:

  import Json.*

  test("Json.jsonParser"):

    assertResult(
      JArray(
        Vector(
          JString("foo"),
          JNumber(666.0),
          JBool(true),
          JBool(false),
          JNull,
          JObject(Map.empty),
          JArray(Vector.empty),
        )
      )
    )( actual =
      """[
        |  "foo",
        |  666,
        |  true,
        |  false,
        |  null,
        |  {},
        |  []
        |]
        |""".stripMargin.asJson
    )

    assertResult(
      JObject(
        Map(
          "a" -> JString("foo"),
          "b" -> JNumber(666.0),
          "c" -> JBool(true),
          "d" -> JBool(false),
          "e" -> JNull,
          "f" -> JObject(Map.empty),
          "g" -> JArray(Vector.empty)
        )
      )
    )(actual =
      """{
        |  "a": "foo",
        |  "b": 666,
        |  "c": true,
        |  "d": false,
        |  "e": null,
        |  "f": {},
        |  "g": []
        |}
        |""".stripMargin.asJson
    )

  test("Json.resolve"):

    val json: Json =
      """{
        |  "a": null,
        |  "b": true,
        |  "c": 6.66,
        |  "d": "foo",
        |  "e": [
        |    {},
        |    {
        |      "aa": null,
        |      "bb": true,
        |      "cc": 3.33,
        |      "dd": "foo",
        |      "ee": [
        |        "e1ee0",
        |        "e1ee1"
        |      ]
        |    }
        |  ],
        |  "f": {
        |    "666": {
        |      "aa": null,
        |      "bb": true,
        |      "cc": 9.99,
        |      "dd": "baz",
        |      "ee": [
        |        "f666ee0",
        |        "f666ee1"
        |      ]
        |    },
        |    "aaa": {
        |      "aa": null,
        |      "bb": false,
        |      "cc": 6.66,
        |      "dd": "bar",
        |      "ee": [
        |        "faaaee0",
        |        "faaaee1"
        |      ]
        |    }
        |  }
        |}
        |""".stripMargin.asJson

    assertResult(Some(json))
      (actual = json.resolve(JsonPointer("/")))
    assertResult(Some(JNull))
      (actual = json.resolve(JsonPointer("/a")))
    assertResult(Some(JBool(true)))
      (actual = json.resolve(JsonPointer("/b")))
    assertResult(Some(JNumber(6.66)))
      (actual = json.resolve(JsonPointer("/c")))
    assertResult(Some(JString("foo")))
      (actual = json.resolve(JsonPointer("/d")))
    assertResult(
      Some(
        JArray(
          IndexedSeq(
            /* index 0 */
            JObject(Map.empty),
            /* index 1 */
            JObject(
              Map(
                "aa" -> JNull,
                "bb" -> JBool(true),
                "cc" -> JNumber(3.33),
                "dd" -> JString("foo"),
                "ee" -> JArray(
                  IndexedSeq(
                    JString("e1ee0"),
                    JString("e1ee1")
                  )
                )
              )
            )
          )
        )
      ))(actual = json.resolve(JsonPointer("/e")))

    assertResult(Some(JObject(Map.empty)))
      (actual = json.resolve(JsonPointer("/e/0")))
    assertResult(Some(JNull))
      (actual = json.resolve(JsonPointer("/e/1/aa")))
    assertResult(Some(JBool(true)))
      (actual = json.resolve(JsonPointer("/e/1/bb")))
    assertResult(Some(JNumber(3.33)))
      (actual = json.resolve(JsonPointer("/e/1/cc")))
    assertResult(Some(JString("foo")))
      (actual = json.resolve(JsonPointer("/e/1/dd")))
    assertResult(Some(JArray(IndexedSeq(JString("e1ee0"), JString("e1ee1")))))
      (actual = json.resolve(JsonPointer("/e/1/ee")))

    assertResult(
      Some(
        JObject(
          Map(
            /* key aaa */
            "aaa" -> JObject(
              Map(
                "aa" -> JNull,
                "bb" -> JBool(false),
                "cc" -> JNumber(6.66),
                "dd" -> JString("bar"),
                "ee" -> JArray(
                  IndexedSeq(
                    JString("faaaee0"),
                    JString("faaaee1")
                  )
                )
              )
            ),
            /* key 666 */
            "666" -> JObject(
              Map(
                "aa" -> JNull,
                "bb" -> JBool(true),
                "cc" -> JNumber(9.99),
                "dd" -> JString("baz"),
                "ee" -> JArray(
                  IndexedSeq(
                    JString("f666ee0"),
                    JString("f666ee1")
                  )
                )
              )
            )
          )
        )
      ))(actual = json.resolve(JsonPointer("/f")))

    assertResult(Some(JNull))
      (actual = json.resolve(JsonPointer("/f/aaa/aa")))
    assertResult(Some(JBool(false)))
      (actual = json.resolve(JsonPointer("/f/aaa/bb")))
    assertResult(Some(JNumber(6.66)))
      (actual = json.resolve(JsonPointer("/f/aaa/cc")))
    assertResult(Some(JString("bar")))
      (actual = json.resolve(JsonPointer("/f/aaa/dd")))
    assertResult(Some(JArray(IndexedSeq(JString("faaaee0"), JString("faaaee1")))))
      (actual = json.resolve(JsonPointer("/f/aaa/ee")))

    assertResult(Some(JNull))
      (actual = json.resolve(JsonPointer("/f/666/aa")))
    assertResult(Some(JBool(true)))
      (actual = json.resolve(JsonPointer("/f/666/bb")))
    assertResult(Some(JNumber(9.99)))
      (actual = json.resolve(JsonPointer("/f/666/cc")))
    assertResult(Some(JString("baz")))
      (actual = json.resolve(JsonPointer("/f/666/dd")))
    assertResult(Some(JArray(IndexedSeq(JString("f666ee0"), JString("f666ee1")))))
      (actual = json.resolve(JsonPointer("/f/666/ee")))

    assertResult(None)
      (actual = json.resolve(JsonPointer("/g")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/9")))

    assertResult(None)
      (actual = json.resolve(JsonPointer("/a/z")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/b/z")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/c/z")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/d/z")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/e/z")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/d/z")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/e/2")))
    assertResult(None)
      (actual = json.resolve(JsonPointer("/f/2")))


    import JsonPointer.Segment.*
    assertResult(Some(JNull))
      (actual = json.resolve(JsonPointer("/{v}/aaa/aa"), Map(Variable("v") -> Name("f"))))
    assertResult(Some(JBool(false)))
      (actual = json.resolve(JsonPointer("/f/{v}/bb"), Map(Variable("v") -> Name("aaa"))))
    assertResult(Some(JNumber(6.66)))
      (actual = json.resolve(JsonPointer("/f/aaa/{v}"), Map(Variable("v") -> Name("cc"))))
    assertResult(Some(JArray(IndexedSeq(JString("faaaee0"), JString("faaaee1")))))
      (actual =
        json.resolve(
          JsonPointer("/{a}/{b}/{c}"),
          Map(
            Variable("a") -> Name("f"),
            Variable("b") -> Name("aaa"),
            Variable("c") -> Name("ee")
          )
        )
      )
