package cannery.models

import cannery.string_templates.StringTemplate

case class Snippet(keywords:Seq[String], snippets:Seq[StringTemplate])

object Snippet{

  type Index = Map[String, Seq[StringTemplate]]

  def growIndex(index: Index, topic: Snippet): Index={
    topic.keywords.foldLeft(index){ (index, keyword) =>
      val current = index getOrElse(keyword, List.empty[StringTemplate])
      index + (keyword -> (topic.snippets++:current))
    }
  }

  def buildIndex(snippets: Seq[Snippet]): Index = {
    //TODO Cats might have a Semigroup for Maps that does this more elegantly.
    snippets.foldLeft(Map.empty[String, Seq[StringTemplate]])(growIndex)

  }
}
