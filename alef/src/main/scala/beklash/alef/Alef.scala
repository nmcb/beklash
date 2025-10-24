package beklash
package alef

import util.*

object Alef extends App:
  
  import Parsers.*

  lazy val modelParser: P[Model] =

    import Model.*

    def token(s: String) =
      string(s).token

    def name: P[String] =
      regex("[a-zA-Z_][a-zA-Z\\d_]*".r) <* whitespace

    def operation: P[String] =
      regex("[+\\-*/]".r) <* whitespace

    def arg: P[Model] =
      token("$") *> name.map(n => Var(n))

    def num: P[Model] =
      double.map(d => Val(d))

    def bin: P[Model] =
      token("(") *> (operation ** (model ** model)).map((o,a) => Bin(o, a._1, a._2)) <* token(")")

    def model: P[Model] =
      bin | arg | num

    (whitespace *> model <* whitespace).root
  
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
          name ->
            modelParser.run(model).fold(
              error => sys.error(s"unable to parse: $path\n${error.toString}"),
              identity)
        .toMap

    Service(models)
