package beklash
package alef

import beklash.util.*

object Alef extends App:

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

  private def number: Parser[Val] =
    for
      _ <- optionalWhitespace
      n <- negativeNumber | positiveNumber
      _ <- optionalWhitespace
    yield
      Val(n)

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

  
  // Alef service
  
  lazy val service: Service =
    import java.nio.file.*
    import java.net.*
    import scala.io.*
    import scala.jdk.CollectionConverters.*

    val running: URL =
      Alef.getClass.getResource("/")

    // Calculate the models resources path depending on whether we're running locally or in a jar?
    val resources: Path =
      if running.getProtocol == "file" then
        Paths.get(running.toURI)
      else
        val strings = running.toString.split("!")
        FileSystems.newFileSystem(URI.create(strings(0)), Map.empty[String,String].asJava).getPath(strings(1))

    val models: Map[String,Model] =
      Files
        .list(resources)
        .iterator
        .asScala
        .filter(_.toString.endsWith(".model"))
        .map: path =>
          val name = path.getFileName.toString
          val model = Source.fromInputStream(Files.newInputStream(path), "UTF-8").getLines.mkString
          name -> Alef.parse(model)
        .toMap

    Service(models)
