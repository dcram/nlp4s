package fr.dcram.nlp4s.automata

import scala.annotation.tailrec

case class RegexMatch[Tok](tokens:Seq[Tok], groups:Map[String, Seq[RegexMatch[Tok]]])

object RegexMatch {
  def apply[Tok](i:AutInst[Tok]):RegexMatch[Tok] = {
    @tailrec
    def collect(matchStack:List[(StateInst[Tok], Match[Tok])], tokens:Seq[Tok], groups:Map[String, Seq[RegexMatch[Tok]]]):(Seq[Tok], Map[String, Seq[RegexMatch[Tok]]]) = {
      matchStack match {
        case (_, EpsilonMatch(_)) :: rest =>
          collect(rest, tokens, groups)
        case (_, TokenMatch(tok, _)) :: rest =>
          tok match {
            case UserToken(t) => collect(rest, t +: tokens, groups)
            case _ => collect(rest, tokens, groups)
          }
        case (_, AutMatch(ti)) :: rest =>
          val subMatch = RegexMatch(ti.autInst)
          ti.autTransit.name match {
            case Some(name) =>
              collect(rest, subMatch.tokens ++ tokens, groups + (name -> (subMatch +: groups.getOrElse(name, Seq.empty))))
            case None =>
              collect(rest, subMatch.tokens ++ tokens , groups ++ subMatch.groups)
          }
        case Nil =>
          (tokens, groups)
        case h :: _ =>
          throw new IllegalStateException(s"Unexpected element: $h")
      }
    }
    val (tokens, groups) = collect(i.matchStack, List.empty, Map.empty)
    new RegexMatch(tokens, groups)
  }
}
