package fr.dcram.nlp4s.automata

import org.scalatest.FunSpec

class AutomataSpec extends FunSpec {
  case class E(c:Char) extends Token
  def sequence(string:String) = string.toCharArray.toSeq.map(E.apply)

  val ScalA = sequence("ScalA")

  object Vowel extends TokenMatcher[E] {
    private[this] val values = "aeiouy".toCharArray.toSet
    override def matches(tok: E): Boolean = values.contains(tok.c.toLower)
  }
  object Consomn extends TokenMatcher[E] {
    override def matches(tok: E): Boolean = !Vowel.matches(tok)
  }

  val A1 = new AutomatonBuilder[E]()
    .initial(1)
    .transition(1,2,Vowel)
    .transition(2,3,Consomn, true)
    .build

  describe("matchesIn") {
    Seq(
      ((ScalA, A1), Some("al"))
    ).foreach{
      case ((sequence, automaton), expected) =>
        assert(seqMatch(sequence, automaton).map(_._2.map(_.c).mkString) == expected)
    }
  }

}
