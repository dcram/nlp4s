package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.automata.RegexMatch
import fr.dcram.nlp4s.model.Token

case class NerMatch(regexMatch:RegexMatch[Token], srcString:String) {

  def textOpt(groupName:String):Option[String] = groups(groupName).headOption.map(_.text)
  def text(groupName:String):String =  groups(groupName).head.text
  def tokens(groupName:String):Iterable[Token] =  groups(groupName).head.regexMatch.tokens
  def tokensOpt(groupName:String):Option[Iterable[Token]] =  groups(groupName).headOption.map(_.regexMatch.tokens)
  def groups(groupName:String):Iterable[NerMatch] = regexMatch.groups.getOrElse(groupName, Iterable.empty).map(rm => NerMatch(rm, srcString))
  def begin:Int = regexMatch.tokens.head.begin
  lazy val end:Int = regexMatch.tokens.last.end
  def text:String =  srcString.substring(begin, end)

}
