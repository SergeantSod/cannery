package cannery.yaml

import java.io.{ByteArrayInputStream, InputStream}

import org.yaml.snakeyaml.Yaml
import java.util.{List => JavaList, Map => JavaMap}

import scala.collection.JavaConverters._

/**
  * A thin wrapper around something that can be represented in snakeYaml with factory methods that make it more typesafe.
  * The factory methods in the companion object are the main API.
  * this is just for testing.
  */
case class YamlFragment private(snakeYamlRepresentation: Any){
  def toYaml: String = {
    YamlFragment.snakeYaml.dump(snakeYamlRepresentation)
  }
}

object YamlFragment{
  private lazy val snakeYaml = new Yaml()

  //TODO We shouldn't take Any, but a YamlGenerator
  def obj(entries: (String, Any)*): YamlFragment = this(Map(entries:_*).asJava)

  def seq(first: YamlFragment, other: YamlFragment*): YamlFragment = seq(first +: other)
  def seq(elements: Seq[YamlFragment]): YamlFragment = this(elements.map(_.snakeYamlRepresentation).asJava)

  implicit def asYaml(fragment: YamlFragment): InputStream = {
    new ByteArrayInputStream(fragment.toYaml.getBytes)
  }
}

