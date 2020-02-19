package fr.dcram.nlp4s.automata

object Quantifiers {
  sealed abstract class Quantifier
  object Star extends Quantifier                  // *
  object Plus extends Quantifier                  // +
  object ZeroOne extends Quantifier               // ?
  case class N(n:Int) extends Quantifier          // {n}
  case class MN(m:Int, n:Int) extends Quantifier {// {m,n}
    require(m>=n, s"Bad {m,n} quatifier. m>=n required. Got: {$m,$n}")
  }
  case class NStar(n:Int) extends Quantifier      // {n,}
  case class ZeroN(n:Int) extends Quantifier      // {,n}
}
