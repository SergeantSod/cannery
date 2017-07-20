package cannery.yaml

import java.io.{ByteArrayInputStream, InputStream}

import org.yaml.snakeyaml.Yaml
import java.util.{List => JavaList, Map => JavaMap}

import scala.collection.JavaConverters._

trait YamlGeneration {

  lazy val snakeYaml = new Yaml()

  //TODO Dry this up with implementation by extracing all of SnakeYamls types into a package/object that can be imported
  type YamlObjectRepresentation = JavaMap[String, Any]

  //TODO potentially make stuff more type safe by hiding the YAMLObjectRepresentation instead.
  def yamlInputFor(yamlObject: YamlObjectRepresentation): InputStream = {
    val stringResult = snakeYaml.dump(yamlObject)
    new ByteArrayInputStream(stringResult.getBytes)
  }

  def obj(entries: (String, Any)*): YamlObjectRepresentation = Map(entries:_*).asJava
}
