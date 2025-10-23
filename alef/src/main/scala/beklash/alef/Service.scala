package beklash
package alef

case class Service(models: Map[String,Model]):

  def call(name: String)(input: Input): Output = {
    models
      .getOrElse(name, sys.error(s"no such service: $name"))
      .interpret(input)
  }


