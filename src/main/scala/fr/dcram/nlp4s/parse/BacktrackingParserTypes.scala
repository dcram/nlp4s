package fr.dcram.nlp4s.parse

object BacktrackingParserTypes {
  type Parser[Tok, +A] = Seq[Tok] => LazyList[Result[Tok, A]]
  case class Result[+Tok, +A](seq:Seq[Tok], m:MatchData[Tok, A])
  case class MatchData[+Tok, +A](data:A, tokens:List[Tok])
  case class ScanResult[+Tok, +A](matchList:List[MatchData[Tok, A]], timedout: Boolean, matchLimitReached: Boolean)

  object ScanResult {
    def ok[Tok, A](matchedData:List[MatchData[Tok, A]]) = ScanResult(matchedData, false, false)
    def timedout[Tok, A](matchedData:List[MatchData[Tok, A]]) = ScanResult(matchedData, true, false)
    def matchLimitReached[Tok, A](matchedData:List[MatchData[Tok, A]]) = ScanResult(matchedData, false, true)
  }
}
