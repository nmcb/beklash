package beklash
package alef

type Value  = Double
type Name   = String
type Input  = Map[Name, Value]
type Error  = String
type Output = Either[Error, Value]

enum Model:
  case Val(v: Value)                     extends Model
  case Bin(op: Name, a: Model, b: Model) extends Model
  case Var(name: Name)                   extends Model

extension (model: Model)

  def interpret(input: Input): Output =
    import Model.*

    def bin(a: Model, b: Model, f: (Value, Value) => Value): Output =
      a.interpret(input).flatMap(v => b.interpret(input).map(f(v,_)))

    model match
      case Val(v) =>
        Right(v)
      case Bin(o, a, b) =>
        o match
          case "*" => bin(a, b, _ * _)
          case "/" => bin(a, b, _ / _)
          case "+" => bin(a, b, _ + _)
          case "-" => bin(a, b, _ - _)
          case _   => Left(s"unknown operation: $o")
      case Var(n) =>
        input.get(n).toRight(s"unresolved variable: $n")
