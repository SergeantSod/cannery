package cannery

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

trait UnitSpec extends FreeSpec with Matchers with TypeCheckedTripleEquals with PropertyChecks  {

}
