package cannery.string_templates

case class StringTemplate(parts: Seq[TemplatePart]){

  def eval(bindings: Bindings): String = {
    parts.map(_.evaluate(bindings)).mkString
  }

  lazy val variableNames:Set[String] = parts.collect{case Variable(name) => name}.toSet

  override lazy val toString: String={
    parts.map(_.toString).mkString
  }

  def concat(other: StringTemplate):StringTemplate = StringTemplate(parts ++ other.parts)
}

object StringTemplate{
  def parse(template: String): StringTemplate = this(TemplatePart.parse(template))

  //TODO this + concat looks like a Monoid
  def empty = this(Seq())
}