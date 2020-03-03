package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.model.Annotation

case class NerAddress(
                    num:Option[String],
                    streetType:Option[String],
                    streetName:Option[String],
                    zip:String,
                    city:String,
                    override val begin:Int,
                    override val end:Int,
                    override val text:String) extends Annotation
