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

  val variablePattern = "<([^<>]+)>".r

  @tailrec
  def parse(template: CharSequence, into: Seq[TemplatePart] = Vector.empty[TemplatePart]): Seq[TemplatePart] = variablePattern findFirstMatchIn template match {
    case None => into :+ Constant(template.toString)
    case Some(matchData) => {
      val prefix = matchData.before
      val variableName = matchData.group(1)

      parse(matchData.after, into :+ Constant(prefix.toString) :+ Variable(variableName) )
    }
  }

}