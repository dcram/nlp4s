package fr.dcram.nlp4s.ner

case class MapResource(uri:String, sep:Char) extends AbstractResource(uri) {

  val asMap:Map[String, String] = source.getLines().map(_.trim)
    .filterNot(_.startsWith("#"))
    .map(_.split(sep))
    .filter(_.length > 1)
    .map(m => m(0) -> m(1))
    .toMap

  def keys:Set[String] = asMap.keySet
  def contains(key:String):Boolean = asMap.contains(key)
  def get(key:String):Option[String] = asMap.get(key)
}

