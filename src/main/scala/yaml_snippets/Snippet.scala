package yaml_snippets

case class Snippet(keywords:Seq[String], text:String){
  lazy val template = StringTemplate.parse(text)
}
