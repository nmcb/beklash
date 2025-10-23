package beklash
package alef

import beklash.util.*

type Input  = Map[String,Int]
type Output = Int

object Alef:

  import Model.*

  // alef language parser

  import Parser.*

  private def negativeNumber: Parser[Int] =
    for
      _ <- char('-')
      i <- digits
    yield
      -i

  private def positiveNumber: Parser[Int] =
    digits

  private def number: Parser[Num] =
    for
      _ <- optionalWhitespace
      n <- negativeNumber | positiveNumber
      _ <- optionalWhitespace
    yield
      Num(n)

  private def operation(o: String): Parser[Bin] =
    for
      _ <- optionalWhitespace
      _ <- char('(')
      _ <- optionalWhitespace
      _ <- keyword(o)
      a <- parser
      _ <- optionalWhitespace
      b <- parser
      _ <- optionalWhitespace
      _ <- char(')')
      _ <- optionalWhitespace
    yield
      Bin(o, a, b)
      
  private def variable: Parser[Var] =
    for 
      _ <- optionalWhitespace
      _ <- char('$')
      v <- string
      _ <- optionalWhitespace
    yield
      Var(v)

  private def parser: Parser[Model] =
    operation("+") | operation("-") | operation("*") | operation("/") | number | variable

  def parse(s: String): Model =
    parser.run(s)
