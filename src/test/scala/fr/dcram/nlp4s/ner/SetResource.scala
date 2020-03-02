package fr.dcram.nlp4s.ner

case class SetResource(uri:String) extends AbstractResource(uri) {
  val asSet:Set[String] = source.getLines().map(_.trim).filterNot(_.startsWith("#")).toSet
  def contains(key:String):Boolean = asSet.contains(key)
}
