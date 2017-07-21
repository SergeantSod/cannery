package cannery
import scala.annotation.tailrec

package object string_templates {
  type Bindings = String => Any

  class MissingVariableException(variable:String) extends RuntimeException(s"Missing binding for variable in string template: $variable.")





}
