package beklash.util

import org.scalatest.funsuite.AnyFunSuite

class TestParser extends AnyFunSuite:

  enum Json:
    case Str(underlying: String)            extends Json
    case Num(underlying: Int)               extends Json
    case Arr(underlying: List[Json])        extends Json
    case Obj(underlying: Map[String, Json]) extends Json

  import Json.*

  object Json:

    import Parser.*

    def string: Parser[String] =
      for
        _ <- optionalWhitespace
        _ <- char('"')
        s <- satisfy(_.isLetter).zeroOrMore
        _ <- char('"')
        _ <- optionalWhitespace
      yield
        s.mkString

    def negativeNumber: Parser[Int] =
      for
        _ <- optionalWhitespace
        _ <- char('-')
        i <- digits
        _ <- optionalWhitespace
      yield
        -i

    def positiveNumber: Parser[Int] =
      digits

    def number: Parser[Int] =
      for
        _ <- optionalWhitespace
        i <- negativeNumber | positiveNumber
        _ <- optionalWhitespace
      yield
        i

    def member: Parser[(String,Json)] =
      for
        _ <- optionalWhitespace
        k <- string
        _ <- optionalWhitespace
        _ <- char(':')
        _ <- optionalWhitespace
        v <- json
        _ <- optionalWhitespace
      yield (k, v)

    def obj: Parser[Json] =
      for
        _  <- optionalWhitespace
        _  <- char('{')
        _  <- optionalWhitespace
        ms <- separated(',', member)
        _  <- optionalWhitespace
        _  <- char('}')
        _  <- optionalWhitespace
      yield
        Obj(ms.toMap)

    def arr: Parser[Json] =
      for
        _  <- optionalWhitespace
        _  <- char('[')
        _  <- optionalWhitespace
        es <- separated(',', json)
        _ <- optionalWhitespace
        _  <- char(']')
        _ <- optionalWhitespace
      yield Arr(es)

    def num: Parser[Json] =
      number.map(i => Num(i))

    def str: Parser[Json] =
      string.map(s => Str(s))

    def json: Parser[Json] =
      arr | obj | num | str

    def parse(s: String): Json =
      json.run(s)


  test("Parser.parse") {
    assertResult(expected = Num(12))(actual   = Json.parse("12"))
    assertResult(expected = Num(12))(actual   = Json.parse(" 12 "))

    assertResult(expected = Num(-1))(actual   = Json.parse("-1"))
    assertResult(expected = Num(-1))(actual   = Json.parse(" -1 "))
    assertResult(expected = Str(""))(actual   = Json.parse("\"\""))
    assertResult(expected = Str(""))(actual   = Json.parse(" \"\" "))

    assertResult(expected = Str("a"))(actual  = Json.parse("\"a\""))
    assertResult(expected = Str("a"))(actual  = Json.parse(" \"a\" "))

    assertResult(expected = Str("ab"))(actual = Json.parse("\"ab\""))
    assertResult(expected = Str("ab"))(actual = Json.parse(" \"ab\" "))

    assertResult(expected = Arr(List(Num(1),Num(2))))(actual = Json.parse("[1,2]"))
    assertResult(expected = Arr(List(Num(1),Num(2))))(actual = Json.parse(" [ 1 , 2 ] "))

    assertResult(expected = Arr(List(Num(1),Obj(Map("a" -> Num(1),"b" -> Num(2))))))(actual = Json.parse("""[1,{"a":1,"b":2}]"""))
    assertResult(expected = Obj(Map("a" -> Num(1),"b" -> Arr(List(Num(1),Num(2))))))(actual = Json.parse("""{"a":1,"b":[1,2]}"""))
    assertResult(expected = Obj(Map("a" -> Num(1),"b" -> Arr(List(Num(1),Num(2))))))(actual = Json.parse(""" { "a" : 1 , "b" : [ 1 , 2 ] } """))
  }
