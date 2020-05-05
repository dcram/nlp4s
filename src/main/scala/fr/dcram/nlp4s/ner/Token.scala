package fr.dcram.nlp4s.ner

trait Annotation {
  val begin:Int
  val end:Int
  val text:String
}

case class Token(
                  override val begin:Int,
                  override val end:Int,
                  override val text:String) extends Annotation
