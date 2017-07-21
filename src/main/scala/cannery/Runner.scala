package cannery

import java.io._
import java.util.{List => JavaList, Map => JavaMap}

import cannery.models.Snippet
import cannery.yaml.YamlReads
import org.yaml.snakeyaml.Yaml

import scala.collection.JavaConverters._
import scala.util.control.Exception.allCatch
import string_templates._
import repl._

object Runner {

  def main(arguments: Array[String]) = arguments match {
    case Array(snippetsFilePath, outputFilePath) => generateFromSnippets(snippetsFilePath, outputFilePath)
    case _ => {
      println("Provide exactly two arguments, one input file with YAML snippets, one output filename to save the result.")
    }
  }

  //TODO Reorder methods
  def loaned[T <: { def close(): Unit }](closable: => T)(block: T => Unit): Unit = {
    var inputStream:Option[T] = None
    try{
      inputStream = Some(closable)
      inputStream.foreach(block)
    } finally {
      inputStream.foreach(_.close())
    }
  }


  def readingFrom[T](filePath: String): ((FileInputStream) => Unit) => Unit = loaned(new FileInputStream(filePath))
  def writingTo[T](filePath: String): ((PrintStream) => Unit) => Unit = loaned(new PrintStream(filePath))


  //TODO Naming
  def generateFromSnippets(snippetsFilePath: String, outputFilePath: String): Unit = {
    readingFrom(snippetsFilePath){ inputStream =>
      //TODO We should parse the snippets before opening the output file.
      writingTo(outputFilePath){ output =>

        implicit val templateReads: YamlReads[StringTemplate] = implicitly[YamlReads[String]] map StringTemplate.parse
        yaml.load[Seq[Snippet]](inputStream) match {
          case Left(error) => println(s"Malformed snippets: $error.")
          case Right(topics) => generateFromTopics(topics, output)
        }
      }
    }
  }

  def buildIndex(snippets: Seq[Snippet]): Map[String, Seq[StringTemplate]] = {
    //TODO Apparently, Cats has a Semigroup for Maps that does this more elegantly.
    snippets.foldLeft(Map.empty[String, Seq[StringTemplate]]){ (index, topic) =>
      topic.keywords.foldLeft(index){ (index, keyword) =>
        val current = index getOrElse(keyword, List.empty[StringTemplate])
        index + (keyword -> (topic.snippet+:current))
      }
    }

  }

  //TODO Naming
  private def generateFromTopics(snippets: Seq[Snippet], output: PrintStream) = {

    //TODO These should actually be wrapped up in a model class
    val index : Map[String, Seq[StringTemplate]] = buildIndex(snippets)
    val topics = index.keys.toSeq.sorted

    var outputTemplate: StringTemplate = StringTemplate(Seq())

    choose(topics, "Topics"){ topic =>
      choose(index(topic), "Snippets"){ chosenSnippet =>
        outputTemplate = outputTemplate concat chosenSnippet
        Stop
      }
      Continue
    }
    val bindings = gather("Details are needed", outputTemplate.variableNames)
    output.println(outputTemplate.evaluate(bindings))

  }

  
}
