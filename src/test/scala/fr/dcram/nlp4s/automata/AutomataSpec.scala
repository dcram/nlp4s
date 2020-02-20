package fr.dcram.nlp4s.automata

import org.scalatest.{FunSpec, Ignore}

class AutomataSpec extends FunSpec {

  case class E(c:Char)
  def sequence(string:String):Seq[E] = string.toCharArray.toSeq.map(E.apply)

  object Vowel extends TokenMatcher[E] {
    private[this] val values = "aeiouy".toCharArray.toSet
    override def matches(tok: E): Boolean =
      values.contains(tok.c.toLower)
  }
  object Consomn extends TokenMatcher[E] {
    override def matches(tok: E): Boolean =
      !Vowel.matches(tok)
  }

  val A1:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .build

  val A2:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true).state(4, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .matcherTransition(3,4,Consomn)
    .build

  import Quantifiers._
  import AutomatonFactory._

  val A3:Automaton[E] = AutomatonFactory.sequence(Seq(
    Vowel,
    quantified(Consomn, Plus)
  ))

  def matchToString(m:Seq[Match[E]]):String = m.map(_.asInstanceOf[TokenMatch[E]].token.c.toString).mkString
  describe("matchesIn") {
    Seq(
      (("ScalA", A1), Some("al"))
    ).foreach{
      case ((string, automaton:Automaton[E]), expected) =>
        val seq = sequence(string)
        ignore(s"should find $expected in $string") {
          val matches = automaton.seqMatch(seq)
          assert(matches.headOption.map(m => matchToString(m)) == expected)
        }
    }
  }
  describe("allPrefixMatches") {
    Seq(
//      (("ScalA", A1), Seq.empty),
//      (("alSca", A1), Seq("al")),
//      (("ScalA", A2), Seq.empty),
//      (("alSca", A2), Seq("alS", "al")),
      (("acccab", A3), Seq("accc", "acc", "ac")),
    ).foreach{
      case ((string, automaton:Automaton[E]), expected) =>
        val seq = sequence(string)
        it(s"should find $expected in $string with automaton $automaton") {
          val matches = automaton.allPrefixMatches(seq)
          assert(matches.map(m => matchToString(m)) == expected)
        }
    }
  }

}
