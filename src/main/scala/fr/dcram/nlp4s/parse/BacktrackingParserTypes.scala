package fr.dcram.nlp4s.parse

object BacktrackingParserTypes {
  type Parser[Tok, +A] = Seq[Tok] => Stream[Result[Tok, A]]
  case class Result[+Tok, +A](seq:Seq[Tok], m:MatchData[Tok, A])
  case class MatchData[+Tok, +A](data:A, tokens:List[Tok])

}
