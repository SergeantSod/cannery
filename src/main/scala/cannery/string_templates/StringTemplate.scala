package cannery.string_templates

case class StringTemplate(source: String, parts: Seq[TemplatePart]){

  lazy val variableNames:Seq[String] = parts.collect{case Variable(name) => name}

  def eval(bindings: Bindings): String = {
    parts.map(_.eval(bindings)).mkString
  }
}

object StringTemplate{
  def parse(template: String): StringTemplate = this(template, TemplatePart.parse(template))
}