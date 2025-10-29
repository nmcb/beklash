package beklash
package util

import JsonPointer.*
import Segment.*

case class JsonPointer(path: List[Segment]):

  def headOption: Option[Segment] =
    path.headOption

  def tail: JsonPointer =
    copy(path = path.tail)

  def variables: List[Segment] =
    path.filter:
      case v: Variable => true
      case _           => false

  def isNormalised: Boolean =
    variables.isEmpty


  def normalise(values: Map[Variable,Name]): Option[JsonPointer] =
    val attempt = copy(path = path.map:
      case v: Variable => values.getOrElse(v, v)
      case s: Segment  => s
    )
    Option.when(attempt.isNormalised)(attempt)


object JsonPointer:

  def apply(path: String): JsonPointer =
    jsonPointerParser.run(path).toTry.get

  enum Segment(val name: String):
    case Name(override val name: String)     extends Segment(name)
    case Variable(override val name: String) extends Segment(name)

  import Segment.*

  import Parsers.*

  def slash: P[Char] =
    char('/')

  def variable: P[Segment] =
    (char('{') *> regex("[^{}/]+".r) <* char('}')).map(Variable.apply)

  def name: P[Segment] =
    regex("[^{}/]+".r).map(Name.apply)

  def jsonPointerParser: P[JsonPointer] =
    slash *> (variable | name).sep(slash).map(JsonPointer.apply)

