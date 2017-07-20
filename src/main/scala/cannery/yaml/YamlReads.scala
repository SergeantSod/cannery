package cannery.yaml

import java.util.{List => JavaList, Map => JavaMap}

import org.yaml.snakeyaml.Yaml
import shapeless.{HList, HNil, LabelledProductTypeClass, LabelledProductTypeClassCompanion, TypeCase, Typeable}
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
trait YamlReads[T] { leftReads =>
  def reads(rawValue: Any): ErrorOr[T]

  def recoveringWith(fallback: YamlReads[T]): YamlReads[T] = new YamlReads[T] {
    override def reads(rawValue: Any): ErrorOr[T] = {
      // Note that we flatMap over the left projection, which effectively allows us to provide an alternative Either
      // that is only evaluated for the failure case. This also means we throw way the error message (which isn't ideal).
      leftReads.reads(rawValue).left.flatMap(_ => fallback.reads(rawValue) )
    }
  }

  //TODO smells like a Functor. If we have Cats, investigate what other type classes we get
  def map[O](transform: T => O): YamlReads[O] = new YamlReads[O] {
    override def reads(rawValue: Any): ErrorOr[O] = {
      leftReads.reads(rawValue).map(transform)
    }
  }
}

//TODO Check if we still need deepCheck.
case class ReadsExpectedType[T:Typeable](nameInError: String, deepCheck:(T=>Boolean) = {_:T => true} ) extends YamlReads[T]{
  override def reads(any: Any): ErrorOr[T] = {
    val IsOfTargetType = TypeCase[T]
    any match {
      case IsOfTargetType(asT) if deepCheck(asT) => Right(asT)
      case _ => Left(s"Expected $nameInError, got ${any.getClass.getName}.")
    }
  }
}

//For instances where there is no Typeable
//TODO Dry it up
case class TypeCastReads[T](nameInError: String)(typeCheck:(Any=>Boolean)) extends YamlReads[T]{
  override def reads(any: Any): ErrorOr[T] = {
    if(typeCheck(any)) { Right(any.asInstanceOf[T]) }
    else Left(s"Expected $nameInError, got ${any.getClass.getName}.")
  }
}

object YamlReads extends LabelledProductTypeClassCompanion[YamlReads] {

  type YamlMap = JavaMap[String, Any]

  //TODO Move typeable instances
  implicit lazy val javaMapTypeAble : Typeable[YamlMap] = new Typeable[YamlMap] {
    override def cast(t: Any): Option[YamlMap] = {
      t match {
        case v: JavaMap[_,_] if v.asScala.keys.forall(_.isInstanceOf[String]) => Some(v.asInstanceOf[YamlMap])
        case _ => None
      }
    }

    override def describe: String = "java.util.Map[String,Any]"
  }

  //TODO If we pull the type in here, we might be able to simplify the code for the generic Reads instance
  implicit lazy val javaListTypeAble : Typeable[JavaList[Any]] = new Typeable[JavaList[Any]] {
    override def cast(t: Any): Option[JavaList[Any]] = {
      t match {
        case v: JavaList[_] => Some(v.asInstanceOf[JavaList[Any]])
        case _ => None
      }
    }

    override def describe: String = "java.util.List[Any]"
  }

  implicit lazy val mapReads:YamlReads[YamlMap] = ReadsExpectedType[YamlMap]("object")

  //TODO support targeting a Scala map with String keys.

  implicit lazy val javaListReads:YamlReads[JavaList[Any]] = ReadsExpectedType[JavaList[Any]]("list")

  implicit lazy val anySeqReads:YamlReads[Seq[Any]] = javaListReads.map(_.asScala)

  implicit lazy val booleanReads: YamlReads[Boolean] = ReadsExpectedType[Boolean]("boolean")

  implicit lazy val intReads: YamlReads[Int] = ReadsExpectedType[Int]("integer")

  implicit lazy val floatReads: YamlReads[Float] = ReadsExpectedType[Float]("float") recoveringWith(doubleReads map {_.toFloat})

  implicit lazy val doubleReads: YamlReads[Double] = ReadsExpectedType[Double]("double")

  implicit lazy val binaryReads: YamlReads[Array[Byte]] = TypeCastReads("binary")(_.isInstanceOf[Array[Byte]])

  // This whole monkey-dance is necessary since not all possible strings are representable as string nodes in yaml,
  // and we allow embedding them as binaries. For this case SnakeYaml returns them as byte arrays.
  //TODO This also still has some encoding issues, but I'm not really certain if those are only coming in from the tests.
  implicit lazy val stringReads: YamlReads[String] = ReadsExpectedType[String]("string") recoveringWith( binaryReads map { new String(_) } )

  implicit def seqReads[T](implicit elements: YamlReads[T]): YamlReads[Seq[T]] = new YamlReads[Seq[T]] {
    override def reads(any: Any): ErrorOr[Seq[T]] = {

      anySeqReads.reads(any).flatMap{ listOfAny:Seq[Any] =>
        val result: ErrorOr[Seq[T]] = Right(Vector.empty[T])
        listOfAny.foldLeft(result){ (current, next) =>
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

    override def emptyProduct: YamlReads[HNil] = mapReads map (_ => HNil)
  }



}
