package yaml_snippets

import java.io._
import java.util.{List => JavaList, Map => JavaMap}

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
      writingTo(outputFilePath){ output =>
        val yaml = new Yaml
        recoverTopics(yaml.load(inputStream)) match {
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
        index + (keyword -> (topic.template+:current))
      }
    }

  }

  //TODO Naming
  private def generateFromTopics(snippets: Seq[Snippet], output: PrintStream) = {
    //TODO Formatting

    val index : Map[String, Seq[StringTemplate]] = buildIndex(snippets)
    val topics = index.keys.toSeq.sorted
    choose(topics, "Topics")(){ topic =>
      choose(index(topic), "Snippets")(_.source){ chosenSnippet =>
        //TODO It would be awesome if there were some way to define global variables, or better yet, re-use earlier bindings where possible and only ask for information on demand.
        val bindings = gather("Details are needed", chosenSnippet.variableNames)
        output.println(chosenSnippet.eval(bindings))
        Stop
      }
      Continue
    }

  }



  //TODO Move all this stuff to a YAML wrapper or replace with Jackson+YAML or use this as an opportunity to play around with shapeless's type-class typeclass and derive a mapper from SnakeYaml that way.
  def recoverTopics(potentialList: Any) : ErrorOr[Seq[Snippet]] = {
    potentialList match {
      case aList:JavaList[_] =>
        val emptyResult: ErrorOr[Vector[Snippet]] = Right(Vector.empty[Snippet])
        aList.asScala.foldLeft(emptyResult){ (resultOption, someObject) =>
          for {
            someResult <- resultOption
            newTopic <- recoverTopic(someObject)
          } yield someResult :+ newTopic
        }
      case _ =>  Left(s"Expected a List of Topics, but got a ${potentialList.getClass}.")
    }
  }

  //TODO Move to YAML wrapper or replace with Jackson+YAML
  def recoverTopic(potentialHashMap: Any) : ErrorOr[Snippet] = {
    potentialHashMap match {
      case aHashMap: JavaMap[Any, Any] => {
        val scalaMap = aHashMap.asScala
        for {
          snippetsObject <- scalaMap get "snippet" toRight "missing snippets key"
          snippetText <- allCatch.either{ snippetsObject.asInstanceOf[String] }.left.map(_ => "Expected a string.")
          keywordsObject <- scalaMap get "keywords" toRight "missing keywords key"
          keywordsSeq <- recoverStringSeq(keywordsObject)
        } yield Snippet(keywordsSeq, snippetText)
      }
      case _ => Left(s"Expected a Map, but got a ${potentialHashMap.getClass}.")
    }
  }

  //TODO Move to YAML wrapper or replace with Jackson+YAML
  def recoverStringSeq(potentialList:Any):ErrorOr[Seq[String]] = {
    potentialList match {
      case aList: JavaList[Any] if aList.asScala.forall(_.isInstanceOf[String]) =>
        Right(aList.asScala.asInstanceOf[Seq[String]])
      case _ => Left(s"Expected a List of Strings, but got a ${potentialList.getClass}.")
    }
  }
  
}
