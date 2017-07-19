package yaml_snippets
import scala.annotation.tailrec

package object string_templates {
  type Bindings = Map[String, Any]

  class MissingVariableException(variable:String) extends RuntimeException(s"Missing binding for variable: $variable.")

  sealed trait TemplatePart{
    def eval(bindings: Map[String, Any]): String
  }

  case class Constant(text: String) extends TemplatePart {
    override def eval(bindings: Bindings): String = text
  }

  case class Variable(name: String) extends TemplatePart {
    override def eval(bindings: Bindings): String = {
      bindings get name match {
        case Some(value) => value.toString()
        case None => throw new MissingVariableException(name)
      }
    }
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


  case class StringTemplate(source: String, parts: Seq[TemplatePart]){

    lazy val variableNames:Seq[String] = parts.collect{case Variable(name) => name}

    def eval(bindings: Bindings): String = {
      parts.map(_.eval(bindings)).mkString
    }
  }

  object StringTemplate{
    def parse(template: String): StringTemplate = this(template, TemplatePart.parse(template))
  }
}
