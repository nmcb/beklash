package beklash
package util

case class JsonPointer(path: List[String]):

  lazy val headOption: Option[String] =
    path.headOption

  lazy val tail: JsonPointer =
    copy(path = path.tail)
    
  def resolve(json: Json): Option[Json] =
    import Json.*

    headOption match
      case None =>
        Some(json)
      case Some(segment) =>
        json match
          case JArray(sequence) =>
            segment
              .toIntOption
              .flatMap(sequence.lift)
              .flatMap(tail.resolve)
          case JObject(objects) =>
            objects
              .get(segment)
              .flatMap(tail.resolve)
          case _ =>
            None


object JsonPointer:

  import Parsers.*

  def slash: P[Char] =
    char('/')

  def jsonPointerParser: P[JsonPointer] =
    slash *> regex("[^/]+".r).sep(slash).map(JsonPointer.apply)

