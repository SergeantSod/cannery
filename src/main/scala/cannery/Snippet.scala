package cannery

import cannery.string_templates.StringTemplate

case class Snippet(keywords:Seq[String], snippet:String){
  lazy val template = StringTemplate.parse(snippet)
}
