package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.parse.ParserTypes.Parser

object NerTypes {
  type TokenParser[+A] = Parser[Token[String], Token[A]]
  type StringTokenizer = String => Stream[Token[String]]


}
