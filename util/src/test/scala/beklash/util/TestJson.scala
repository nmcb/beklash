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

  test("Json.resolve"):
    val json: Json =
      JObject(
        Map(
          "a" -> JNull,
          "b" -> JBool(true),
          "c" -> JNumber(6.66),
          "d" -> JString("foo"),
          "e" -> JArray(
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
          ),
          "f" -> JObject(
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
        )
      )

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
  