package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.model.Annotation

case class Title(text: String, lemma:String)
case class NerPerson(
                      title:Option[Title],
                      firstname:Option[String],
                      lastname:String,
                      override val begin:Int,
                      override val end:Int,
                      override val text:String) extends Annotation