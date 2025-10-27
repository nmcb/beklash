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
  case class  Or(l: Expression, r: Expression)                                  extends Expression
  case class  And(l: Expression, r: Expression)                                 extends Expression
  case class  Equality(o: EqualityOp, l: Expression, r: Expression)             extends Expression
  case class  Relational(o: RelationalOp, l: Expression, r: Expression)         extends Expression
  case class  Additive(o: AdditiveOp, l: Expression, r: Expression)             extends Expression
  case class  Multiplicative(o: MultiplicativeOp, l: Expression, r: Expression) extends Expression
  case class  QualifiedPathExpression(p: JsonPath)                              extends Expression
  case class  NumberExpression(d: Double)                                       extends Expression
  case class  StringExpression(s: String)                                       extends Expression
  case class  BooleanExpression(b: Boolean)                                     extends Expression
  case object NullExpression                                                    extends Expression

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
  case object Nil              extends JsonPath
  case class  Root(step: Step) extends JsonPath
  case class  Step(test: JPTest, predicate: Option[Predicate], location: JPLocation, tail: JsonPath) extends JsonPath

  enum JPLocation:
    case Relative
    case Recursive

  enum JPTest:
    case JPTypeTest(t: String) extends JPTest
    case JPNameTest(n: String) extends JPTest

  import JPTest.*

  import Parsers.*

  def jsonPathParser: P[JsonPath] =
    path

  def token(s: String) =
    string(s).token

  def path: P[JsonPath] =
    absolutePath | relativePath

  def absolutePath: P[JsonPath] =
    string("$") *> qualifiedPath.map(Root.apply)

  def qualifiedPath: P[Step] =
    string(".") *> (
      (string(".") *> relativePath).map(_.copy(location = JPLocation.Recursive))
        | relativePath.map(_.copy(location = JPLocation.Relative))
      )

  def relativePath: P[Step] =
    (step ** qualifiedPath.opt).map:
      case ((test, predicate), Some(child)) => Step(test, predicate, JPLocation.Relative, child)
      case ((test, predicate), None)        => Step(test, predicate, JPLocation.Relative, Nil)

  def step: P[(JPTest,Option[Predicate])] =
      nodeTest ** predicate.opt

  def nodeTest: P[JPTest] =
    nodeTypeTest | nameTest

  private def nodeTypeTest: P[JPTest] =
      string("object()").as(JPTypeTest("object"))
      | string("array()").as(JPTypeTest("array"))
      | string("string()").as(JPTypeTest("string"))
      | string("boolean()").as(JPTypeTest("boolean"))
      | string("nu") *> (
        string("ll()").as(JPTypeTest("null"))
        | string("mber()").as(JPTypeTest("number"))
      )

  private def nameTest: P[JPTest] =
    (token("*") | name).map(JPNameTest.apply)

  def name: P[String] =
    quoted('\'')

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
      case (l,Some(o,r)) => Multiplicative(o, l, r)
      case (l,None)      => l

  def multiplicativeOp: P[MultiplicativeOp] =
    token("*").as(Multiply) | token("/").as(Divide) | token("%").as(Modulus)

  def unaryExpression: P[Expression] =
    (token("@") *> qualifiedPath).map(QualifiedPathExpression.apply)
    | numberExpression
    | stringExpression
    | booleanExpression
    | nullExpression

  def numberExpression: P[NumberExpression] =
    double.map(NumberExpression.apply)

  def stringExpression: P[StringExpression] =
    quoted('"').map(StringExpression.apply)

  def booleanExpression: P[BooleanExpression] =
    token("true").map(_ => BooleanExpression(true)) | token("false").map(_ => BooleanExpression(false))

  def nullExpression: P[NullExpression.type] =
    token("null").as(NullExpression)

  def signedInteger: P[Int] =
    (token("-").opt ** integer).map:
      case (Some("-"),i) => -i
      case (_,i)         =>  i

  def integer: P[Int] =
    token("+").opt *> digits.map(_.toInt)


  def fromString(s: String): Either[Parsers.Error,JsonPath] =
    jsonPathParser.run(s)

