package beklash
package util

enum Json:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[Json])
  case JObject(get: Map[String,Json])
  
  import JsonPointer.*
  import Segment.*

  def resolve(pointer: JsonPointer, variables: Map[Variable,Name] = Map.empty): Option[Json] =

    import Json.*
    
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

object Json:

  import Parsers.*

  def jsonParser: P[Json] =

    def token(s: String) = string(s).token

    def arr: P[Json] = (
      token("[") *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)) <* token("]")
      ).scope("array")

    def obj: P[Json] = (
      token("{") *> member.sep(token(",")).map(kvs => JObject(kvs.toMap)) <* token("}")
      ).scope("object")

    def member: P[(String, Json)] =
      (quoted('"') <* whitespace) ** (token(":") *> value <* whitespace)

    def literal: P[Json] =
      ( token("null").as(JNull)
      | double.map(JNumber(_))
      | quoted('"').map(JString(_))
      | token("true").as(JBool(true))
      | token("false").as(JBool(false))
      ).scope("literal")

    def value: P[Json] = literal | obj | arr

    (whitespace *> (obj | arr)).root

  def fromString(s: String): Either[Parsers.Error,Json] =
    jsonParser.run(s)

