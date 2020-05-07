package fr.dcram.nlp4s.parse

object ParserTypes {
  type Parser[Tok, +A] = Seq[Tok] => Stream[Result[Tok, A]]
  case class Result[+Tok, +A](seq:Seq[Tok], m:A)

}
