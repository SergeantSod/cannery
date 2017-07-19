package yaml_snippets
import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

package object repl {
  sealed trait Command
  object Continue extends Command
  object Stop extends Command

  //TODO This could benefit from Show (probably in cats)
  @tailrec
  def choose[T](options: Seq[T], header:String)(format: T => String ={ t: T => t.toString})(onChoice: T => Command): Unit ={

    clear()
    printHeader(header)
    println("0: Done.")

    options.zipWithIndex.foreach{ case (option, index) =>
      println(s"${index + 1}: ${format(option)}")
    }

    readInt() match {
      case Left(error) => {
        println(error)
        choose(options, header)(format)(onChoice)
      }
      case Right(0) => {}//0 is the entry for done, so we're done.
      case Right(choice) if (choice >= 1 && choice <= options.size) => {
        val chosen = options(choice - 1 )
        onChoice(chosen) match {
          case Stop => ()
          case Continue =>  choose(options, header)(format)(onChoice)
        }
      }
      case _ => {
        println("Not a valid choice.")
        choose(options, header)(format)(onChoice)
      }
    }

  }

  def gather(prompt: String, keys: Seq[String]):Map[String, String] = {
    if(!keys.isEmpty){
      clear()
      printHeader(prompt)
    }
    keys.map{ key =>
      println(s"$key:")
      key -> StdIn.readLine()
    }.toMap
  }
  //TODO The terminal-related stuff could be extracted.
  def printHeader(header: String):Unit = {
    println(Console.UNDERLINED + header)
  }

  def clear():Unit = {
    print("\033[2J")
  }

  def readInt(): ErrorOr[Int] = {
    Try{
      StdIn.readInt()
    }.toEither.left.map(_ => "Not a valid number.")
  }
}
