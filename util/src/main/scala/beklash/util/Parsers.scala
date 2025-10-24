package beklash
package util

import java.util.regex.*
import scala.util.matching.Regex

trait Parsers[Parser[+_]]:

  def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def defaultSucceed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  def regex(r: Regex): Parser[String]

  def whitespace: Parser[String] = regex("\\s*".r)

  def digits: Parser[String] = regex("\\d+".r)

  def thru(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r)

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def doubleString: Parser[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  def double: Parser[Double] =
    doubleString.map(_.toDouble).label("double literal")

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  extension [A](p: Parser[A])

    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def attempt: Parser[A]

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.product(p2).map((a, b) => f(a, b))

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def slice: Parser[String]

    def opt: Parser[Option[A]] =
      p.map(Some(_)) | succeed(None)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap(a => p2.map(b => (a, b)))

    def **[B](p2: => Parser[B]): Parser[(A,B)] = product(p2)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def label(msg: String): Parser[A]

    def scope(msg: String): Parser[A]

    def *>[B](p2: => Parser[B]) =
      p.slice.map2(p2)((_, b) => b)

    def <*(p2: => Parser[Any]) =
      p.map2(p2.slice)((a, b) => a)

    def token: Parser[A] = p.attempt <* whitespace

    def sep(separator: Parser[Any]): Parser[List[A]] =
      p.sep1(separator) | succeed(Nil)

    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    def as[B](b: B): Parser[B] = p.slice.map(_ => b)

    def opL(op: Parser[(A, A) => A]): Parser[A] =
      p.map2((op ** p).many)((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

    def root: Parser[A] =
      p <* eof


case class Location(input: String, offset: Int = 0):

  lazy val line: Int =
    input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col: Int =
    input.slice(0, offset + 1).lastIndexOf('\n') match
      case -1 => offset + 1
      case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) =
    copy(offset = offset + n)

  def remaining: String =
    input.substring(offset)

  def slice(n: Int) =
    input.substring(offset, offset + n)

  def currentLine: String =
    if input.length > 1
    then
      val itr = input.linesIterator.drop(line - 1)
      if (itr.hasNext) itr.next() else ""
    else ""

  def columnCaret: String =
    (" " * (col - 1)) + "^"

case class ParseError(stack: List[(Location, String)] = Nil):
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map((loc, msg) => s"${formatLoc(loc)} $msg").mkString("\n") + context

  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String =
    s"${l.line}.${l.col}"