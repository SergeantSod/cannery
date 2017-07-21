package cannery.yaml

import java.util.{List => JavaList, Map => JavaMap}

import org.yaml.snakeyaml.Yaml
import shapeless.{:+:, CNil, Coproduct, HList, HNil, Inr, LabelledProductTypeClass, LabelledProductTypeClassCompanion, LabelledTypeClass, LabelledTypeClassCompanion, TypeCase, Typeable}
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


//TODO Dry it up
case class TypeCastReads[T](nameInError:String, cast: Any => Option[T]) extends YamlReads[T]{

  override def reads(any: Any): ErrorOr[T] = {
    cast(any).toRight(s"Expected ${nameInError}, got ${any.getClass.getName}.")
  }
}

object TypeCastReads{

  //For instances where there is no Typeable
  def apply[T](nameInError:String)(cast: PartialFunction[Any,T]):TypeCastReads[T] = {
    this(nameInError, cast.lift)
  }

  def apply[T](implicit typeable:Typeable[T]):TypeCastReads[T]={
    this(typeable.describe, typeable.cast)
  }
}

object YamlReads extends LabelledTypeClassCompanion[YamlReads] {

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

  implicit lazy val mapReads:YamlReads[YamlMap] = TypeCastReads[YamlMap]

  //TODO support targeting a Scala map with String keys.

  implicit lazy val javaListReads:YamlReads[JavaList[Any]] = TypeCastReads[JavaList[Any]]

  implicit lazy val anySeqReads:YamlReads[Seq[Any]] = javaListReads.map(_.asScala)

  implicit lazy val booleanReads: YamlReads[Boolean] = TypeCastReads[Boolean]

  implicit lazy val intReads: YamlReads[Int] = TypeCastReads[Int]

  implicit lazy val floatReads: YamlReads[Float] = TypeCastReads[Float] recoveringWith(doubleReads map {_.toFloat})

  implicit lazy val doubleReads: YamlReads[Double] = TypeCastReads[Double]

  implicit lazy val binaryReads: YamlReads[Array[Byte]] = TypeCastReads("binary"){
    //Arrays are not Typeable by design: https://github.com/milessabin/shapeless/issues/465

    //But we can do a pattern match since Arrays don't suffer from type erasure
    case b:Array[Byte] => b
  }

  // This whole monkey-dance is necessary since not all possible strings are representable as string nodes in yaml,
  // and we allow embedding them as binaries. For this case SnakeYaml returns them as byte arrays.
  //TODO This also still has some encoding issues, but I'm not really certain if those are only coming in from the tests.
  implicit lazy val stringReads: YamlReads[String] = TypeCastReads[String] recoveringWith( binaryReads map { new String(_) } )

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

  val typeClass: LabelledTypeClass[YamlReads] = new LabelledTypeClass[YamlReads] {

    override def product[H, T <: HList](key: String, head: YamlReads[H], tail: YamlReads[T]): YamlReads[shapeless.::[H, T]] = new YamlReads[shapeless.::[H, T]] {
      override def reads(rawValue: Any): ErrorOr[shapeless.::[H, T]] = {

        for {
          map <- mapReads.reads(rawValue)
          scalaMap = map.asScala
          //TODO If we want to support Option, failing here in the case of a missing key is a little bit too eager.
          //The reads should have to say a word about this and receive the optional value. This means that we'd have to change the interface of Reads, though.
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

    override def coproduct[L, R <: Coproduct](name: String, firstReads: => YamlReads[L], otherReads: => YamlReads[R]): YamlReads[:+:[L, R]] = new YamlReads[:+:[L, R]]{

      override def reads(rawValue: Any): ErrorOr[L:+:R] = {


        val firstResult: ErrorOr[L:+:R] = firstReads.reads(rawValue).map{Coproduct[L:+:R](_)}
        for {
          _ <- firstResult.left
          otherResult <- otherReads.reads(rawValue)
        } yield Inr(otherResult)
      }
    }

    override def emptyCoproduct: YamlReads[CNil] = new YamlReads[CNil] {
      override def reads(rawValue: Any): ErrorOr[CNil] = Left("No matching case.")
    }
  }



}
