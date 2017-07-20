package cannery.yaml

import cannery.UnitSpec

class YamlSpec extends UnitSpec with YamlGeneration {
  "Reading Yaml" - {

    "when parsing a case class with String fields" - {
      case class CaseClassWithStringField(text: String)

      "should successfully parse a yaml document with a single object" in {
        forAll{ someText: String =>
          load[CaseClassWithStringField]( yamlInputFor(obj("text" -> someText)) ) should ===(Right(CaseClassWithStringField(someText)))
        }
      }

      "should successfully parse a yaml document with additional fields" in {
        forAll{ (someText: String, someOtherText: String )  =>
          load[CaseClassWithStringField]( yamlInputFor(obj("text" -> someText, "other" -> someOtherText)) ) should ===(Right(CaseClassWithStringField(someText)))
        }
      }

      "should return a failure when the field is missing from the yaml" in {
        load[CaseClassWithStringField]( yamlInputFor(obj("other" -> "text")) ) shouldBe a[Left[_, _]]
      }

    }
  }
}
