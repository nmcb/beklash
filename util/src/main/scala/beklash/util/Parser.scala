package beklash.util

case class Parser[+A](parse: String => Option[(A,String)]):

  def run(s: String): A =
    parse(s) match
      case Some(a,   "") => a
      case Some(_, rest) => sys.error(s"unconsumed at ${rest.take(10)}")
      case None          => sys.error(s"failed to parse")

  def map[B](f: A => B): Parser[B] =
    Parser(s => parse(s) match
      case Some(a, r) => Some(f(a), r)
      case None       => None
    )

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(s => parse(s) match
      case Some(a, r) => f(a).parse(r)
      case None       => None
    )

  private def loop[X >: A](s: String, acc: List[X] = List.empty): (List[X], String) =
    parse(s) match
      case None         => (acc.reverse, s)
      case Some((a,ss)) => loop(ss, a :: acc)

  def zeroOrMore: Parser[List[A]] =
    Parser(s => Some(loop(s)))

  def oneOrMore: Parser[List[A]] =
    Parser(s => parse(s).flatMap((a, ss) => Some(loop(ss, List(a)))))

  def |[A1 >: A](that: => Parser[A1]): Parser[A1] =
    Parser(s => parse(s) match {
      case None        => that.parse(s)
      case res@Some(_) => res
    })

  def ~[B](that: => Parser[B]): Parser[B] =
    for { _ <- this ; b <- that } yield b


object Parser:

  def unit[A](a: A): Parser[A] =
    Parser(s => Some(a, s))

  def fail[A]: Parser[A] =
    Parser(_ => None)

  def take: Parser[Char] =
    Parser(s => if s.nonEmpty then Some(s.head, s.tail) else None)

  def satisfy(p: Char => Boolean): Parser[Char] =
    take.flatMap(c => if p(c) then unit(c) else fail)

  def char(c: Char): Parser[Char] =
    satisfy(_ == c)

  def digit: Parser[Char] =
    satisfy(_.isDigit)

  def digits: Parser[Int] =
    digit.oneOrMore.map(_.mkString("").toInt)

  def separated[A](sep: Char, pa: => Parser[A]): Parser[List[A]] =
    for { h <- pa ; t <- (char(sep) ~ pa).zeroOrMore } yield h :: t
