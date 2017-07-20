package cannery.yaml

import java.util.{List => JavaList, Map => JavaMap}

import org.yaml.snakeyaml.Yaml
import shapeless.{HList, HNil, LabelledProductTypeClass, LabelledProductTypeClassCompanion}
import cannery.ErrorOr
import cannery.yaml.YamlReads.YamlMap

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

/**
  * A type class to convert from snakeyamls object-representation into a desired type.
  * Returns an Either that wraps the result on success or a string describing the nature of the error.
  *
  * @tparam T The desired type.
  */
trait YamlReads[T] {
  def reads(rawValue: Any): ErrorOr[T]
}

trait Helpers{

  def expectWithCheck[T:ClassTag](nameInError:String, any: Any)(deepCheck:T=>Boolean): ErrorOr[T] ={
    any match {
      case asT:T if deepCheck(asT) => Right(asT)
      case _ => Left(s"Expected $nameInError, got ${any.getClass.getName}.")
    }
  }

  def expectYamlMap(any: Any): ErrorOr[YamlMap] = expectWithCheck[YamlMap]("object", any){ _.asScala.keys.forall(_.isInstanceOf[String]) }

  def expect[T:ClassTag](nameInError:String, any: Any): ErrorOr[T] = expectWithCheck[T](nameInError, any)(_ => true)

}


object YamlReads extends LabelledProductTypeClassCompanion[YamlReads] {

  type YamlMap = JavaMap[String, Any]

  implicit val mapReads:YamlReads[YamlMap] = new YamlReads[YamlMap] with Helpers {
    override def reads(any: Any): ErrorOr[YamlMap] = expectYamlMap(any)
  }

  implicit val stringReads: YamlReads[String] = new YamlReads[String] with Helpers {
    override def reads(rawValue: Any): ErrorOr[String] = {
      // This whole monkey-dance is necessary since not all possible strings are representable as string nodes in yaml,
      // and we allow embedding them as binaries. For this case SnakeYaml returns them as byte arrays.

      // Note that we flatMap over the left projection, which effectively allows us to provide an alternative Either
      // that is only evaluated for the failure case. This also explains why we throw way the error message.
      //TODO This also still has some encoding issues, but I'm not really certain if those are only coming in from the tests.
      for {
        _ <- expect[String]("string", rawValue).left
        binary <- expect[Array[Byte]](s"binary for string", rawValue)
      } yield new String(binary)
    }

  }

  implicit def seqReads[T](implicit elements: YamlReads[T]): YamlReads[Seq[T]] = new YamlReads[Seq[T]] with Helpers {
    override def reads(any: Any): ErrorOr[Seq[T]] = {
      expect[JavaList[Any]]("list", any).flatMap{ listOfAny =>
        val result: ErrorOr[Seq[T]] = Right(Vector.empty[T])
        listOfAny.iterator.asScala.foldLeft(result){ (current, next) =>
          for {
            result <- current
            nextElement <- elements.reads(next)
          } yield result :+ nextElement
        }
      }


    }
  }

  val typeClass: LabelledProductTypeClass[YamlReads] = new LabelledProductTypeClass[YamlReads] {

    override def product[H, T <: HList](key: String, head: YamlReads[H], tail: YamlReads[T]): YamlReads[shapeless.::[H, T]] = new YamlReads[shapeless.::[H, T]] {
      override def reads(rawValue: Any): ErrorOr[shapeless.::[H, T]] = {

        for {
          map <- implicitly[YamlReads[YamlMap]].reads(rawValue)
          scalaMap = map.asScala
          headAny <- scalaMap.get(key).toRight(s"Mandatory key $key is missing.")
          headValue <- head.reads(headAny)
          tailValue <- tail.reads(map)
        } yield shapeless.::(headValue, tailValue)

      }
    }

    override def project[F, G](instance: => YamlReads[G], to: (F) => G, from: (G) => F): YamlReads[F] = new YamlReads[F] {
      override def reads(rawValue: Any): ErrorOr[F] = {
        instance.reads(rawValue).map(from)
      }
    }

    override def emptyProduct: YamlReads[HNil] = new YamlReads[HNil] with Helpers {
      override def reads(any: Any): ErrorOr[HNil] = {
        expectYamlMap(any).map{ _ => HNil }
      }
    }
  }



}
