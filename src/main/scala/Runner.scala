import java.io._
import java.util
import java.util.{List => JavaList}
import java.util.{Map => JavaMap}
import javax.script.Bindings

import org.yaml.snakeyaml.Yaml

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.util.control.Exception.allCatch

object Runner {

  def main(arguments: Array[String]) = arguments match {
    case Array(snippetsFilePath, outputFilePath) => generateFromSnippets(snippetsFilePath, outputFilePath)
    case _ => {
      println("Provide exactly two arguments, one input file with YAML snippets, one output filename to save the result.")
    }
  }

  //TODO Dry up the loan pattern.
  def readinFrom[T](filePath: String)(block: InputStream => T ):Unit = {
    var inputStream:Option[InputStream] = None
    try{
      inputStream = Some(new BufferedInputStream(new FileInputStream(filePath)))
      inputStream.foreach(block)
    } finally {
      inputStream.foreach(_.close())
    }

  }

  //TODO Dry up the loan pattern.
  def writingTo[T](filePath: String)(block: PrintStream => T ):Unit = {
    var outputStream:Option[PrintStream] = None
    try{
      outputStream = Some(new PrintStream(filePath))
      outputStream.foreach(block)
    } finally {
      outputStream.foreach(_.close())
    }

  }

  //TODO Naming
  def generateFromSnippets(snippetsFilePath: String, outputFilePath: String): Unit = {
    readinFrom(snippetsFilePath){ inputStream =>
      writingTo(outputFilePath){ output =>
        val yaml = new Yaml
        recoverTopics(yaml.load(inputStream)) match {
          case Left(error) => println(s"Malformed snippets: $error.")
          case Right(topics) => generateFromTopics(topics, output)
        }
      }
    }
  }

  def buildIndex(snippets: Seq[Snippet]): Map[String, Seq[String]] = {
    val emptyResult = Map.empty[String, Seq[String]] withDefaultValue Seq.empty[String]
    snippets.foldLeft(Map.empty[String, Seq[String]]){ (index, topic) =>
      topic.keywords.foldLeft(index){ (index, keyword) =>
        val current = index getOrElse(keyword, List.empty[String])
        index + (keyword -> (topic.text+:current))
      }
    }

  }

  //TODO Naming
  private def generateFromTopics(snippets: Seq[Snippet], output: PrintStream) = {
    import REPL._
    //TODO Formatting

    val index : Map[String, Seq[String]] = buildIndex(snippets)
    val topics = index.keys.toSeq.sorted
    choose(topics, "Topics")(){ topic =>
      choose(index(topic), "Snippets")() { chosenSnippet =>
        output.println(chosenSnippet)
        Stop
      }
      Continue
    }

  }

  object REPL extends Enumeration{
    type Command = Value
    val Continue, Stop = Value

    @tailrec
    def choose[T](options: Seq[T], header:String)(format: T => String ={ t: T => t.toString})(onChoice: T => Command): Unit ={
      println(header)
      println("0: Done.")

      options.zipWithIndex.foreach{ case (option, index) =>
        println(s"${index + 1}: ${format(option)}")
      }

      val choice = StdIn.readInt()

      if(choice == 0){}
      else if (choice >= 1 && choice <= options.size){
        val chosen = options(choice - 1 )
        onChoice(chosen) match {
          case Continue =>  choose(options, header)(format)(onChoice)
          case Stop => ()
        }
      } else {
        println("Not a valid choice.")
        choose(options, header)(format)(onChoice)
      }
    }
  }


  //TODO Move
  type Result[T] = Either[String, T]

  //TODO Move
  def recoverTopics(potentialList: Any) : Result[Seq[Snippet]] = {
    potentialList match {
      case aList:JavaList[_] =>
        val emptyResult: Result[Vector[Snippet]] = Right(Vector.empty[Snippet])
        aList.asScala.foldLeft(emptyResult){ (resultOption, someObject) =>
          for {
            someResult <- resultOption
            newTopic <- recoverTopic(someObject)
          } yield someResult :+ newTopic
        }
      case _ =>  Left(s"Expected a List of Topics, but got a ${potentialList.getClass}.")
    }
  }

  //TODO Move
  def recoverTopic(potentialHashMap: Any) : Result[Snippet] = {
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

  //TODO Move
  def recoverStringSeq(potentialList:Any):Result[Seq[String]] = {
    potentialList match {
      case aList: JavaList[Any] if aList.asScala.forall(_.isInstanceOf[String]) =>
        Right(aList.asScala.asInstanceOf[Seq[String]])
      case _ => Left(s"Expected a List of Strings, but got a ${potentialList.getClass}.")
    }
  }

  object string_templates{


  }
}
