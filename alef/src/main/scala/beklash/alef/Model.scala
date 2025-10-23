package beklash
package alef

enum Model:
  case Num(i: Int)                         extends Model
  case Bin(op: String, a: Model, b: Model) extends Model
  case Var(name: String)                   extends Model

extension (model: Model)

  def interpret(input: Map[String,Int]): Int =
    import Model.*
    model match
      case Num(i) =>
        i
      case Bin(o, a, b) =>
        o match
          case "*" => a.interpret(input) * b.interpret(input)
          case "/" => a.interpret(input) / b.interpret(input)
          case "+" => a.interpret(input) + b.interpret(input)
          case "-" => a.interpret(input) - b.interpret(input)
          case _   => sys.error(s"unknown operation: $o")
      case Var(v) if input.contains(v) =>
        input(v)
      case Var(v) =>
        sys.error(s"unresolved variable: $v")
