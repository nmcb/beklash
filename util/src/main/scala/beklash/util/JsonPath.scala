package beklash
package util

object JsonPath:

  trait Predicate
  case object Wildcard                            extends Predicate
  case class Subscript(index: Int)                extends Predicate
  case class Slice(from: Int, to: Int, step: Int) extends Predicate
  case class Filter(expression: Expression)       extends Predicate

  trait Union                                                                extends Predicate
  case class SubscriptUnion(l: Int, r: Int)                                  extends Union
  case class PathUnion(l: JsonPath, r: JsonPath)                             extends Union
  case class FilterUnion(l: JsonPath | Expression, r: JsonPath | Expression) extends Union

  trait Expression
  case class Or(l: Expression, r: Expression)                          extends Expression
  case class And(l: Expression, r: Expression)                         extends Expression
  case class Equality(o: EqualityOp, l: Expression, r: Expression)     extends Expression
  case class Relational(o: RelationalOp, l: Expression, r: Expression) extends Expression
  case class Additive(o: AdditiveOp, l: Expression, r: Expression)     extends Expression
  case class QualifiedPathExpression(p: JsonPath)                      extends Expression

  sealed trait EqualityOp
  case object Equals        extends EqualityOp
  case object EqualsNot     extends EqualityOp

  sealed trait RelationalOp

  case object LargerThan        extends RelationalOp
  case object LessThan          extends RelationalOp
  case object LargerOrEqualThan extends RelationalOp
  case object LessOrEqualThan   extends RelationalOp

  sealed trait AdditiveOp
  case object Plus  extends AdditiveOp
  case object Minus extends AdditiveOp

  sealed trait MultiplicativeOp
  case object Multiply extends MultiplicativeOp
  case object Divide   extends MultiplicativeOp
  case object Modulus  extends MultiplicativeOp

  trait JsonPath
  case object Nil                                                           extends JsonPath
  case class Step(test: Test, predicate: Option[Predicate], tail: JsonPath) extends JsonPath

  trait Test
  case object Object            extends Test
  case object Array             extends Test
  case object String            extends Test
  case object Number            extends Test
  case object Boolean           extends Test
  case object Null              extends Test
  case class Name(name: String) extends Test

  import Parsers.*

  def jsonPathParser: P[JsonPath] =

    def token(s: String) = string(s).token

    def path: P[JsonPath] =
      absolutePath | relativePath

    def absolutePath: P[JsonPath] =
      token("$") *> qualifiedPath

    def qualifiedPath: P[JsonPath] =
      recursiveLocation | recursiveLocation

    def recursiveLocation: P[JsonPath] =
      token("..") *> relativePath

    def relativeLocation: P[JsonPath] =
      token(".") *> relativePath

    def relativePath: P[JsonPath] =
      (step ** qualifiedPath).map:
        case ((test, predicate), child) => Step(test, predicate, child)

    def step: P[(Test,Option[Predicate])] =
        (nodeTest ** predicate.opt)

    def nodeTest: P[Test] =
      (nodeType <* token("()")) | nameTest

    def nodeType: P[Test] =
      token("object").as(Object)
      | token("array").as(Array)
      | token("string").as(String)
      | token("number").as(Number)
      | token("boolean").as(Boolean)
      | token("null").as(Null)

    def nameTest: P[Test] =
      (token("*") | name).map(Name.apply)

    def name: P[String] =
        quoted('\'') *> regex("[^']+".r) <* quoted('\'')

    def predicate: P[Predicate] =
      token("[") *> (wildcard | subscript | slice | union | filter) <* token("]")

    def wildcard: P[Predicate] =
      token("*").as(Wildcard)

    def subscript: P[Subscript] =
      signedInteger.map(Subscript.apply)

    def slice: P[Slice] =
      def argument: P[Int] = token("[") *> signedInteger <* token("]")
      (argument ** argument ** argument).map:
        case ((from, to), step) => Slice(from, to, step)

    def union: P[Union] =
      (integer ** token(",") ** integer).map:
        case ((l,_),r) => SubscriptUnion(l, r)
      | (unionExpression ** token(",") ** unionExpression).map:
        case ((l,_),r) => FilterUnion(l, r)

    def unionExpression: P[JsonPath | Expression] =
      relativePath | filterExpression

    def filter: P[Filter] =
      (token("?(") *> filterExpression <* token(")")).map(Filter.apply)

    def filterExpression: P[Expression] =
      orExpression

    def orExpression: P[Expression] =
      (andExpression ** (token("or") ** orExpression).opt).map:
        case (l,Some((_,r))) => Or(l, r)
        case (l,None)        => l

    def andExpression: P[Expression] =
      (equalityExpression ** (token("and") ** andExpression).opt).map:
        case (l,Some((_,r))) => And(l, r)
        case (l,None)        => l

    def equalityExpression: P[Expression] =
      (relationalExpression ** (equalityOp ** equalityExpression).opt).map:
        case (l,Some(o,r)) => Equality(o, l, r)
        case (l,None)      => l

    def equalityOp: P[EqualityOp] =
      token("==").as(Equals) | token("!=").as(EqualsNot)


    def relationalExpression: P[Expression] =
      (additiveExpression ** (relationalOp ** relationalExpression).opt).map:
        case (l,Some(o,r)) => Relational(o,l,r)
        case (l,None)      => l

    def relationalOp: P[RelationalOp] =
      token(">").as(LargerThan)
      | token("<").as(LessThan)
      | token(">=").as(LargerOrEqualThan)
      | token("<=").as(LessOrEqualThan)

    def additiveExpression: P[Expression] =
      (multiplicativeExpression ** (additiveOp ** additiveExpression).opt).map:
        case (l,Some(o,r)) => Additive(o, l, r)
        case (l,None)      => l

    def additiveOp: P[AdditiveOp] =
      token("+").as(Plus) | token("-").as(Minus)

    def multiplicativeExpression: P[Expression] =
      (unaryExpression ** (multiplicativeOp ** multiplicativeExpression).opt).map:
        ???

    def multiplicativeOp: P[MultiplicativeOp] =
      token("*").as(Multiply) | token("/").as(Divide) | token("%").as(Modulus)

    def unaryExpression: P[Expression] =
      (token("@") *> qualifiedPath).map(QualifiedPathExpression.apply)
      // TODO others


    def signedInteger: P[Int] =
      (token("-").opt ** integer).map:
        case (Some("-"),i) => -i
        case (_,i)         =>  i

    def integer: P[Int] =
      token("+").opt *> digits.map(_.toInt)

    ???

  def fromString(s: String): Either[Parsers.Error,JsonPath] =
    jsonPathParser.run(s)

