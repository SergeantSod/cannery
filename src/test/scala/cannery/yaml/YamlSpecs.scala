package cannery.yaml

import cannery.UnitSpec
import cannery.yaml.YamlFragment._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.nonEmptyListOf

class YamlSpec extends UnitSpec {
  "Reading Yaml" - {

    case class CaseClassWithStringField(someField: String)

    "when parsing a case class with String fields" - {

      "when parsing a single object" - {

        "should successfully parse a yaml document with a single object" in {
          forAll{ someText: String =>
            load[CaseClassWithStringField](obj("someField" -> someText)) should ===(Right(CaseClassWithStringField(someText)))
          }
        }

        "should successfully parse a yaml document with additional fields" in {
          forAll{ (someText: String, someOtherText: String )  =>
            load[CaseClassWithStringField]( obj("someField" -> someText, "other" -> someOtherText) ) should ===(Right(CaseClassWithStringField(someText)))
          }
        }

        "should return a failure when the field is missing from the yaml" in {
          load[CaseClassWithStringField]( obj("other" -> "text") ) shouldBe a[Left[_, _]]
        }

        "should return a failure when the top-level object in the yaml is a sequence" in {
          load[CaseClassWithStringField]( seq(obj("someField" -> "text")) ) shouldBe a[Left[_, _]]
        }

      }

      "when parsing a sequence of objects" - {
        "should successfully parse sequences of well-formed objects" in {
          forAll{ someStrings: Seq[String] =>

            val yamlRepresentations = someStrings.map{ s => obj("someField" -> s) }
            val objectRepresentations = someStrings.map(CaseClassWithStringField)

            load[Seq[CaseClassWithStringField]](seq(yamlRepresentations)) should ===(Right(objectRepresentations))
          }

        }

        "should return a failure for sequences of objects with missing keys" in {
          //Note that we need a non-empty list to have something in it that actually turns the input invalid.
          forAll(nonEmptyListOf(arbitrary[String])){ someStrings =>

            val yamlRepresentations = someStrings.map{ s => obj("other" -> s) }

            load[Seq[CaseClassWithStringField]](seq(yamlRepresentations)) shouldBe a[Left[_, _]]
          }
        }
      }

    }
  }
}
