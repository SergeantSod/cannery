package cannery.string_templates

import scala.annotation.tailrec

sealed trait TemplatePart{
  def evaluate(bindings: Bindings): String
}

case class Constant(text: String) extends TemplatePart {
  override def evaluate(bindings: Bindings): String = text
  override val toString:String = text
}

case class Variable(name: String) extends TemplatePart {
  override def evaluate(bindings: Bindings): String = {
    bindings(name).toString
  }

  override lazy val toString:String = s"<$name>"
}

object TemplatePart {

  def parse(template: String): Seq[TemplatePart] = parseInto(template: String, Vector.empty[TemplatePart])

  val variablePattern = "<([^<>]+)>".r

  @tailrec
  private def parseInto(template: CharSequence, result: Seq[TemplatePart]): Seq[TemplatePart] = variablePattern findFirstMatchIn template match {
    case None => result :+ Constant(template.toString)
    case Some(matchData) => {
      val prefix = matchData.before
      val variableName = matchData.group(1)

      parseInto(matchData.after, result :+ Constant(prefix.toString) :+ Variable(variableName) )
    }
  }

}