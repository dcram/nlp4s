package fr.dcram.nlp4s.model

trait Annotation {
  val begin:Int
  val end:Int
  val text:String
}