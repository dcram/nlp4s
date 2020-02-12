package fr.dcram.nlp4s.automata

import org.scalatest.FunSpec

class AutomataSpec extends FunSpec {

  case class E(c:Char)
  def sequence(string:String):Seq[E] = string.toCharArray.toSeq.map(E.apply)

  private val ScalA = sequence("ScalA")

  object Vowel extends TokenMatcher[E] {
    private[this] val values = "aeiouy".toCharArray.toSet
    override def matches(tok: E): Boolean = values.contains(tok.c.toLower)
  }
  object Consomn extends TokenMatcher[E] {
    override def matches(tok: E): Boolean = !Vowel.matches(tok)
  }

  val A1:Automaton[E] = new AutomatonBuilder[E]()
    .initial(1)
    .transition(1,2,Vowel)
    .transition(2,3,Consomn, toStateAccepting = true)
    .build

  def matchToString(m:Seq[Match[E]]):String = m.map(_.asInstanceOf[TokenMatch[E]].token.c.toString).mkString
  describe("matchesIn") {
    Seq(
      ((ScalA, A1), Some("al"))
    ).foreach{
      case ((sequence:Seq[E], automaton:Automaton[E]), expected) =>
        it(s"should find $expected in ${sequence.map(_.c.toString).mkString}") {
          val matches = automaton.seqMatch(sequence)
          assert(matches.headOption.map(m => matchToString(m)) == expected)
        }
    }
  }

}
