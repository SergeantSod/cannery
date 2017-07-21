package cannery.models

import cannery.string_templates.StringTemplate

case class SnippetGroup(keywords:Seq[String], snippets:Seq[StringTemplate])

object SnippetGroup{

  type Index = Map[String, Seq[StringTemplate]]

  def growIndex(index: Index, group: SnippetGroup): Index={
    group.keywords.foldLeft(index){ (index, keyword) =>
      val current = index getOrElse(keyword, List.empty[StringTemplate])
      index + (keyword -> (group.snippets++:current))
    }
  }

  def buildIndex(groups: Seq[SnippetGroup]): Index = {
    //TODO Cats might have a Semigroup for Maps that does this more elegantly.
    groups.foldLeft(Map.empty[String, Seq[StringTemplate]])(growIndex)

  }
}
