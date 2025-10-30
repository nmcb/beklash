package beklash
package util

enum Json:
  case JNull
  case JNumber(n: Double)
  case JString(s: String)
  case JBool(b: Boolean)
  case JArray(js: IndexedSeq[Json])
  case JObject(ms: Map[String,Json])

  import JsonPointer.*
  import Segment.*

  def resolve(pointer: JsonPointer, variables: Map[Variable,Name] = Map.empty): Option[Json] =

    def recurse(json: Json)(normalised: JsonPointer): Option[Json] =
        normalised.headOption match
          case None =>
            Some(json)
          case Some(segment) =>
            json match
              case JArray(sequence) =>
                for
                  index <- segment.name.toIntOption
                  next  <- sequence.lift(index)
                  value <- recurse(next)(normalised.tail)
                yield
                  value
              case JObject(objects) =>
                for
                  next  <- objects.get(segment.name)
                  value <- recurse(next)(normalised.tail)
                yield
                  value
              case _ =>
                None

    pointer.normalise(variables).flatMap(recurse(this))

  def asText: String =
    this match
      case JNull        => "null"
      case JNumber(n)   => n.toString
      case JString(s)   => s"\"$s\""
      case JBool(b)     => b.toString
      case JArray(a)    => a.map(_.asText).mkString("[",",","]")
      case JObject(m)   => m.toList.sortBy(_.key).map((k,v) => s"\"$k\":${v.asText}").mkString("{",",","}")


object Json:
  
  type Member = (String,Json)
  
  extension (m: Member)
    def key: String = m._1
    def value: Json = m._2

  import Parsers.*

  def jsonParser: P[Json] =

    def token(s: String): P[String] =
      string(s) <* whitespace

    def arr: P[Json] = (
      token("[") *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)) <* token("]")
      ).scope("array")

    def obj: P[Json] = (
      token("{") *> member.sep(token(",")).map(kvs => JObject(kvs.toMap)) <* token("}")
      ).scope("object")

    def member: P[(String, Json)] =
      (quoted('"') <* whitespace) ** (token(":") *> value)

    def literal: P[Json] =
      ( token("null").as(JNull)
      | double.map(JNumber(_))
      | quoted('"').map(JString(_))
      | token("true").as(JBool(true))
      | token("false").as(JBool(false))
      ).scope("literal")

    def value: P[Json] = whitespace *> (obj | arr | literal) <* whitespace

    (whitespace *> (obj | arr) <* whitespace).root

  def fromString(s: String): Either[Parsers.Error,Json] =
    jsonParser.run(s)

  extension (s: String) def asJson: Json =
    fromString(s) match
      case Right(json) => json
      case Left(error) => throw error
    
