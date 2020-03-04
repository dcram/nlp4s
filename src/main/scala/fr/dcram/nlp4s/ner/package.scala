package fr.dcram.nlp4s

import fr.dcram.nlp4s.model.Token

import scala.util.matching.Regex
import scala.language.implicitConversions

package object ner {

  case class RegexMatcher(r:Regex) extends NerTokenMatcher {
    override def matches(tok: Token): Boolean = r.findFirstIn(tok.text).nonEmpty
  }
  case class TxtMatcher(matchesStr:String => Boolean) extends NerTokenMatcher {
    override def matches(tok: Token): Boolean = matchesStr(tok.text)
  }

  class AbstractSetMatcher(f:String => String, values: Iterable[String]) extends NerTokenMatcher {
    private val valueSet = values.map(f).toSet
    override def matches(tok: Token): Boolean = valueSet.contains(f(tok.text))
    def lower = new AbstractSetMatcher(_.toLowerCase, values)
  }
  case class SetMatcher(values:String*) extends AbstractSetMatcher(s => s, values)

  case class StringMatcher(str:String) extends NerTokenMatcher {
    override def matches(tok: Token): Boolean = tok.text == str
  }
  implicit def toStringMatcher(str:String):StringMatcher = StringMatcher(str)
}
