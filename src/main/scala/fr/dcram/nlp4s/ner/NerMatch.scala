package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.automata.RegexMatch
import fr.dcram.nlp4s.model.Token

case class NerMatch(m:RegexMatch[Token]) {

  private[this] val JoinChar = " "

  def textOpt(group:String):Option[String] = m.groups.get(group).flatMap(_.headOption).map(_.tokens.map(_.text).mkString(JoinChar))
  def text(group:String):String =  m.groups(group).head.tokens.map(_.text).mkString(JoinChar)
  def begin:Int = m.tokens.head.begin
  lazy val end:Int = m.tokens.last.end
  def text:String =  m.tokens.map(_.text).mkString(JoinChar)

}
