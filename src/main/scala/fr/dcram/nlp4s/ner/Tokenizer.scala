package fr.dcram.nlp4s.ner

import scala.util.matching.Regex

case class Tokenizer(regex:Regex) {
  def tokenize(string:String):Stream[Token] = {
    regex.findAllMatchIn(string).map(m => Token(m.start, m.end, m.group(0)))
  }.toStream
}
