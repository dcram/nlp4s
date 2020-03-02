package fr.dcram.nlp4s

import fr.dcram.nlp4s.model.Token

import scala.util.matching.Regex
import scala.language.implicitConversions

package object ner {

  case class RegexMatcher(r:Regex) extends NerTokenMatcher {
    override def matches(tok: Token): Boolean = r.findFirstIn(tok.text).nonEmpty
  }
  case class TxtMatcher(f:String => Boolean) extends NerTokenMatcher {
    override def matches(tok: Token): Boolean = f(tok.text)
  }
  case class SetMatcher(values:String*) extends NerTokenMatcher {
    val valueSet = Set(values)
    override def matches(tok: Token): Boolean = values.contains(tok.text)
  }
  case class StringMatcher(str:String) extends NerTokenMatcher {
    override def matches(tok: Token): Boolean = tok.text == str
  }
  implicit def toStringMatcher(str:String):StringMatcher = StringMatcher(str)
}
