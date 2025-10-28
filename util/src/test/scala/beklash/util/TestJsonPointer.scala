package beklash.util

import org.scalatest.funsuite.AnyFunSuite

import scala.io.*
import scala.util.*

class TestJsonPointer extends AnyFunSuite:

  import Json.*
  import JsonPointer.*

  val parser = jsonPointerParser

  test("JsonPointer.jsonPointerParser"):
    assertResult(Right(JsonPointer(Nil)))
      (actual = parser.run("/"))
    assertResult(Right(JsonPointer(List("books","2","price"))))
      (actual = parser.run("/books/2/price"))

  test("JsonPointer.resolve"):

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
      (actual = parser.run("/").toOption.flatMap(_.resolve(json)))

    assertResult(Some(JNull))
      (actual = parser.run("/a").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JBool(true)))
      (actual = parser.run("/b").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JNumber(6.66)))
      (actual = parser.run("/c").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JString("foo")))
      (actual = parser.run("/d").toOption.flatMap(_.resolve(json)))

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
      ))(actual = parser.run("/e").toOption.flatMap(_.resolve(json)))

    assertResult(Some(JObject(Map.empty)))
      (actual = parser.run("/e/0").toOption.flatMap(_.resolve(json)))

    assertResult(Some(JNull))
      (actual = parser.run("/e/1/aa").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JBool(true)))
      (actual = parser.run("/e/1/bb").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JNumber(3.33)))
      (actual = parser.run("/e/1/cc").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JString("foo")))
      (actual = parser.run("/e/1/dd").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JArray(IndexedSeq(JString("e1ee0"), JString("e1ee1")))))
      (actual = parser.run("/e/1/ee").toOption.flatMap(_.resolve(json)))

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
      ))(actual = parser.run("/f").toOption.flatMap(_.resolve(json)))

    assertResult(Some(JNull))
      (actual = parser.run("/f/aaa/aa").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JBool(false)))
      (actual = parser.run("/f/aaa/bb").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JNumber(6.66)))
      (actual = parser.run("/f/aaa/cc").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JString("bar")))
      (actual = parser.run("/f/aaa/dd").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JArray(IndexedSeq(JString("faaaee0"), JString("faaaee1")))))
      (actual = parser.run("/f/aaa/ee").toOption.flatMap(_.resolve(json)))

    assertResult(Some(JNull))
      (actual = parser.run("/f/666/aa").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JBool(true)))
      (actual = parser.run("/f/666/bb").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JNumber(9.99)))
      (actual = parser.run("/f/666/cc").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JString("baz")))
      (actual = parser.run("/f/666/dd").toOption.flatMap(_.resolve(json)))
    assertResult(Some(JArray(IndexedSeq(JString("f666ee0"), JString("f666ee1")))))
      (actual = parser.run("/f/666/ee").toOption.flatMap(_.resolve(json)))

    assertResult(None)
      (actual = parser.run("/g").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/9").toOption.flatMap(_.resolve(json)))

    assertResult(None)
      (actual = parser.run("/a/z").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/b/z").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/c/z").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/d/z").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/e/z").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/f/z").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/e/2").toOption.flatMap(_.resolve(json)))
    assertResult(None)
      (actual = parser.run("/f/2").toOption.flatMap(_.resolve(json)))
