package cannery.yaml

import cannery.UnitSpec
import cannery.yaml.YamlFragment._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.nonEmptyListOf

class YamlSpec extends UnitSpec {
  "Reading Yaml" - {

    def parsesSimpleCaseClass[TTarget : YamlReads, TField : Arbitrary](constructor: TField => TTarget)(inYaml:TField => YamlFragment):Unit = {
      "when parsing a single object" - {

        //TODO Hunt down variable names and descriptions that imply everthing is a string.

        "should successfully parse a yaml document with a single object" in {
          forAll{ someFieldValue: TField =>
            load[TTarget](obj("someField" -> inYaml(someFieldValue))) should ===(Right(constructor(someFieldValue)))
          }
        }

        "should successfully parse a yaml document with additional fields" in {
          forAll{ (someFieldValue: TField, otherFieldValue: TField )  =>
            load[TTarget]( obj("someField" -> inYaml(someFieldValue), "other" -> inYaml(otherFieldValue)) ) should ===(Right(constructor(someFieldValue)))
          }
        }

        "should return a failure when the field is missing from the yaml" in {
          forAll{ otherFieldValue : TField =>
            load[TTarget]( obj("more" -> str("stuff"), "other" -> inYaml(otherFieldValue)) ) shouldBe a[Left[_, _]]
          }
        }

        "should return a failure when a well-formed object is wrapped unexpectedly" in {
          forAll { someFieldValue: TField =>
            load[TTarget](seq(obj("someField" -> inYaml(someFieldValue)))) shouldBe a[Left[_, _]]
          }
        }

      }

      "when parsing a sequence of objects" - {
        "should successfully parse sequences of well-formed objects" in {
          forAll{ someFieldValues: Seq[TField] =>

            val yamlRepresentations = someFieldValues.map{ s => obj("someField" -> inYaml(s)) }
            val expectedObjectRepresentations = someFieldValues.map(constructor)

            load[Seq[TTarget]](seq(yamlRepresentations)) should ===(Right(expectedObjectRepresentations))
          }

        }

        "should return a failure for sequences of objects with missing keys" in {
          //Note that we need a non-empty list to have something in it that actually turns the input invalid.
          forAll(nonEmptyListOf(arbitrary[TField])){ someFieldValues =>

            val yamlRepresentations = someFieldValues.map{ s => obj("other" -> inYaml(s)) }

            load[Seq[TTarget]](seq(yamlRepresentations)) shouldBe a[Left[_, _]]
          }
        }
      }
    }

    case class CaseClassWithGenericField[T](someField: T)

    "when parsing a case class with a String field" - {

      "for a simple case class" - {
        case class CaseClassWithStringField(someField: String)
        behave like parsesSimpleCaseClass(CaseClassWithStringField)(str)
      }

      "for a generic case class" - {
        behave like parsesSimpleCaseClass(CaseClassWithGenericField[String])(str)
      }

    }

    "when parsing a case class with a Boolean field" - {

      "for a simple case class" - {
        case class CaseClassWithBooleanField(someField: Boolean)
        behave like parsesSimpleCaseClass(CaseClassWithBooleanField)(bool)
      }

      "for a generic case class" - {
        behave like parsesSimpleCaseClass(CaseClassWithGenericField[Boolean])(bool)
      }

    }

    "when parsing a case class with an Int field" - {

      "for a simple case class" - {
        case class CaseClassWithIntField(someField: Int)
        behave like parsesSimpleCaseClass(CaseClassWithIntField)(int)
      }

      "for a generic case class" - {
        behave like parsesSimpleCaseClass(CaseClassWithGenericField[Int])(int)
      }

    }

    "when parsing a case class with a Float field" - {

      "for a simple case class" - {
        case class CaseClassWithFloatField(someField: Float)
        behave like parsesSimpleCaseClass(CaseClassWithFloatField)(float)
      }

      "for a generic case class" - {
        behave like parsesSimpleCaseClass(CaseClassWithGenericField[Float])(float)
      }

    }

    "when parsing a case class with a Double field" - {

      "for a simple case class" - {
        case class CaseClassWithDoubleField(someField: Double)
        behave like parsesSimpleCaseClass(CaseClassWithDoubleField)(double)
      }

      "for a generic case class" - {
        behave like parsesSimpleCaseClass(CaseClassWithGenericField[Double])(double)
      }

    }

    "when parsing a case class with a nested case class field" - {

      case class NestedCaseClass(value: Int)

      implicit lazy val arbitraryNestedValues = Arbitrary{ arbitrary[Int].map(NestedCaseClass) }

      def withinYaml(nested: NestedCaseClass):YamlFragment = obj("value" -> int(nested.value))

      "for a simple case class" - {
        case class CaseClassWithNestedCaseClassField(someField: NestedCaseClass)
        behave like parsesSimpleCaseClass(CaseClassWithNestedCaseClassField)(withinYaml)
      }

      "for a generic case class" - {
        behave like parsesSimpleCaseClass(CaseClassWithGenericField[NestedCaseClass])(withinYaml)
      }

    }
  }
}
