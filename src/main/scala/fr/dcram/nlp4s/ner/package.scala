package fr.dcram.nlp4s

import scala.language.implicitConversions
import scala.util.matching.Regex

package object ner
  extends Tokenizers
    with TokenParsers
    with StringImprovements {

  import NerTypes._
  object TokenizerRef extends Tokenizers
  object TokenParsersRef extends TokenParsers

  def regexTokenizer(regex:Regex):StringTokenizer = string => regex.findAllMatchIn(string).map(m => Token(m.start, m.end, m.group(0))).to(LazyList)

}
