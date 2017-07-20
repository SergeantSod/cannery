package cannery

import java.io.{FileInputStream, InputStream}

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor


package object yaml {

  private lazy val snakeYaml = new Yaml(new SafeConstructor)


  def load[T](inputStream: InputStream)(implicit reads: YamlReads[T]): ErrorOr[T] = {
    reads.reads( snakeYaml.load(inputStream) )
  }

}
