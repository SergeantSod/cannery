package cannery

import java.io.{FileInputStream, InputStream}

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor

/**
  * A thin, scala-ish wrapper around snakeyaml to parse yaml to simple case classes.
  */
package object yaml {

  // We use SafeConstructor to opt out of arbitrary object generation via yaml (because it generate arbitrary objects,
  // which we don't know how to handle via YamlReads, and because it's dubious feature from a safety perspective anyway).
  private lazy val snakeYaml = new Yaml(new SafeConstructor)

  /**
    * Attempt to parse the content of the given stream as yaml and return it in a representation that has
    * the desired type.
    *
    * @param inputStream The input stream to read from
    * @param reads       The typeclass that targets the desired type
    * @tparam T          The desired type.
    * @return            An Either that wraps the instance of T that captures the contents of the input stream or an error message.
    */
  def load[T](inputStream: InputStream)(implicit reads: YamlReads[T]): ErrorOr[T] = {
    reads.reads( snakeYaml.load(inputStream) )
  }

}
