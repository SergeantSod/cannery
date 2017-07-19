package yaml_snippets

import yaml_snippets.string_templates.StringTemplate

case class Snippet(keywords:Seq[String], text:String){
  lazy val template = StringTemplate.parse(text)
}
