package beklash
package util

import java.util.regex.*
import scala.util.matching.Regex

object Parsers:

  type P[+A] = SrcPos => Result[A]

  case class SrcPos(source: String, offset: Int = 0):

    lazy val line: Int =
      source.slice(0, offset + 1).count(_ == '\n') + 1

    lazy val col: Int =
      source.slice(0, offset + 1).lastIndexOf('\n') match
        case -1        => offset + 1
        case lineStart => offset - lineStart

    def toError(msg: String): Error =
      Error(List((this, msg)))

    def advanceBy(n: Int): SrcPos =
      copy(offset = offset + n)

    def remaining: String =
      source.substring(offset)

    def slice(n: Int): String =
      source.substring(offset, offset + n)

    def currentLine: String =
      if source.length > 1 then
        val lines = source.linesIterator.drop(line - 1)
        if lines.hasNext then
          lines.next()
        else ""
      else ""

    def columnCaret: String =
      (" " * (col - 1)) + "^"

  case class Error(stack: List[(SrcPos,String)] = Nil) extends Exception:

    override def getMessage: String =
      toString

    def push(loc: SrcPos, msg: String): Error =
      copy(stack = (loc, msg) :: stack)

    def label(s: String): Error =
      Error(latestLoc.map((_, s)).toList)

    def latest: Option[(SrcPos,String)] =
      stack.lastOption

    def latestLoc: Option[SrcPos] =
      latest map (_._1)

    override def toString: String =
      if stack.isEmpty then
        "no error message"
      else
        val collapsed: List[(SrcPos,String)] =
          collapseStack(stack)

        val context: String =
          collapsed.lastOption.map((p,_) => "\n\n" + p.currentLine).getOrElse("") +
            collapsed.lastOption.map((p,_) => "\n" + p.columnCaret).getOrElse("")

        collapsed.map((p,m) => s"${formatLoc(p)} $m").mkString("\n") + context

    def collapseStack(stack: List[(SrcPos,String)]): List[(SrcPos,String)] =
      stack
        .groupBy((p,_) => p)
        .view
        .mapValues(_.map((_,m) => m).mkString("; "))
        .toList
        .sortBy((p,_) => p.offset)

    def formatLoc(l: SrcPos): String =
      s"${l.line}.${l.col}"

  enum Result[+A]:
    case Success(get: A, length: Int)
    case Failure(get: Error, isCommitted: Boolean) extends Result[Nothing]

    def extract: Either[Error, A] =
      this match
        case Failure(e,_) => Left(e)
        case Success(a,_) => Right(a)

    def uncommit: Result[A] =
      this match
        case Failure(e, true) => Failure(e, false)
        case _                => this

    def addCommit(isCommitted: Boolean): Result[A] =
      this match
        case Failure(e, c) => Failure(e, c || isCommitted)
        case _             => this

    def mapError(f: Error => Error): Result[A] =
      this match
        case Failure(e, c) => Failure(f(e), c)
        case _             => this

    def advanceSuccess(n: Int): Result[A] =
      this match
        case Success(a,m) => Success(a,n+m)
        case _            => this

  import Result.*

  def succeed[A](a: A): P[A] =
    _ => Success(a, 0)

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while i + offset < s1.length && i < s2.length do
      if s1.charAt(i + offset) != s2.charAt(i) then
        return i
      i += 1
    if s1.length - offset >= s2.length then
      -1
    else
      s1.length - offset

  def string(w: String): P[String] =
    l =>
      val i = firstNonmatchingIndex(l.source, w, l.offset)
      if i == -1 then
        Success(w, w.length)
      else
        Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  def regex(r: Regex): P[String] =
    l => r.findPrefixOf(l.remaining) match
      case None    => Failure(l.toError(s"regex $r"), false)
      case Some(m) => Success(m, m.length)

  def fail(msg: String): P[Nothing] =
    l => Failure(l.toError(msg), true)

  def char(c: Char): P[Char] =
    string(c.toString).map(_.charAt(0))

  def defaultSucceed[A](a: A): P[A] =
    string("").map(_ => a)

  def whitespace: P[String] =
    regex("\\s*".r)

  def digits: P[String] =
    regex("\\d+".r)

  def thru(s: String): P[String] =
    regex((".*?" + Pattern.quote(s)).r)

  def quoted(quote: Char): P[String] =
    string(s"$quote") *> thru(s"$quote").map(_.dropRight(1))

  def doubleString: P[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  def double: P[Double] =
    doubleString.map(_.toDouble).label("double literal")

  def eof: P[String] =
    regex("\\z".r).label("unexpected trailing characters")


  extension [A](p: P[A])

    def run(s: String): Either[Error,A] =
      p(SrcPos(s)).extract

    infix def or(p2: => P[A]): P[A] =
      l => p(l) match
        case Failure(e, false) => p2(l)
        case r                 => r

    def |(p2: => P[A]): P[A] =
      p or p2

    def attempt: P[A] =
      l => p(l).uncommit

    def flatMap[B](f: A => P[B]): P[B] =
      l => p(l) match
        case Success(a,n) =>
          f(a)(l.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case f @ Failure(_,_) => f

    def slice: P[String] =
      l => p(l) match
        case Success(_, n)     => Success(l.slice(n), n)
        case f @ Failure(_, _) => f

    def many: P[List[A]] =
      l =>
        val buf = new collection.mutable.ListBuffer[A]
        def go(p: P[A], offset: Int): Result[List[A]] =
          p(l.advanceBy(offset)) match
            case Success(a, n) =>
              buf += a
              go(p, offset + n)
            case Failure(e, true) => Failure(e, true)
            case Failure(_, _)    => Success(buf.toList, offset)
        go(p, 0)

    def scope(msg: String): P[A] =
      l => p(l).mapError(_.push(l, msg))

    def label(msg: String): P[A] =
      l => p(l).mapError(_.label(msg))

    def listOfN(n: Int): P[List[A]] =
      if n <= 0 then
        succeed(Nil)
      else
        p.map2(p.listOfN(n - 1))(_ :: _)

    def map[B](f: A => B): P[B] =
      p.flatMap(f andThen succeed)

    def map2[B,C](p2: => P[B])(f: (A,B) => C): P[C] =
      p.product(p2).map((a, b) => f(a, b))

    def many1: P[List[A]] =
      p.map2(p.many)(_ :: _)

    def opt: P[Option[A]] =
      p.map(Some(_)) | succeed(None)

    def product[B](p2: => P[B]): P[(A,B)] =
      p.flatMap(a => p2.map(b => (a, b)))

    def **[B](p2: => P[B]): P[(A,B)] =
      product(p2)

    def *>[B](p2: => P[B]) =
      p.slice.map2(p2)((_, b) => b)

    def <*(p2: => P[Any]) =
      p.map2(p2.slice)((a, b) => a)

    def token: P[A] =
      p.attempt <* whitespace

    def sep(separator: P[Any]): P[List[A]] =
      p.sep1(separator) | succeed(Nil)

    def sep1(separator: P[Any]): P[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    def as[B](b: B): P[B] =
      p.slice.map(_ => b)

    def opL(op: P[(A,A) => A]): P[A] =
      p.map2((op ** p).many)((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

    def root: P[A] =
      p <* eof
