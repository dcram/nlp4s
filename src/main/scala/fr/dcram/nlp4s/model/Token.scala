package fr.dcram.nlp4s.model

case class Token(override val begin:Int, override val end:Int, override val text:String) extends Annotation
