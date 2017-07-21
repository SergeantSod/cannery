package cannery
import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

package object repl {
  sealed trait Command
  object Continue extends Command
  object Stop extends Command

  //TODO This could benefit from Show (probably in cats)
  @tailrec
  def choose[T](options: Seq[T], header:String)(onChoice: T => Command): Unit ={

    clear()
    printHeader(header)
    printOption("0", "Done.")

    options.zipWithIndex.foreach{ case (option, index) =>
      printOption(index + 1, option)
    }

    readInt() match {
      case Left(error) => {
        println(error)
        choose(options, header)(onChoice)
      }
      case Right(0) => {}//0 is the entry for done, so we're done.
      case Right(choice) if (choice >= 1 && choice <= options.size) => {
        val chosen = options(choice - 1 )
        onChoice(chosen) match {
          case Stop => ()
          case Continue =>  choose(options, header)(onChoice)
        }
      }
      case _ => {
        println("Not a valid choice.")
        choose(options, header)(onChoice)
      }
    }

  }

  def gather(prompt: String, keys: Set[String]):Map[String, String] = {
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
    println(Console.UNDERLINED + header + Console.RESET)
  }

  def printOption(label:Any, text: Any):Unit = {
    println(Console.BOLD +  s"$label: " + Console.RESET + text)
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
