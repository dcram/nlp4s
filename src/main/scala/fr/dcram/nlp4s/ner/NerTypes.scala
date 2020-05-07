package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.automata.AutomataReferenceTypes

object NerTypes {
  type TokenParser[+A] = AutomataReferenceTypes.Parser[Token[String], Token[A]]
  type StringTokenizer = String => Stream[Token[String]]

}
