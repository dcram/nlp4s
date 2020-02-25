package fr.dcram.nlp4s.automata

import org.scalatest.FunSpec

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

  // Vowel Consomn
  val A1:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .build

  // Vowel Consomn Consomn?
  val A2:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true).state(4, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .matcherTransition(3,4,Consomn)
    .build


  // Vowel Consomn+
  val A3:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .matcherTransition(3,3,Consomn)
    .build

  // Vowel (toto: Vowel Consomn) Vowel
  val A4:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .transition(1,2,Vowel)
    .transition(2,3, A1)
    .transition(3,4,Vowel)
    .build

  // Vowel (toto: Vowel Consomn)? Vowel
  val A5:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .transition(1,2,Vowel)
    .transition(2,3, A1)
    .epsilon(2,3)
    .transition(3,4,Vowel)
    .build

  // Vowel (toto: Vowel Consomn)? Vowel
  val A6:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .transition(1,2,Vowel)
    .namedTransition("toto", 2,3, A1)
    .epsilon(2,3)
    .transition(3,4,Vowel)
    .build

  def matchToString(m:RegexMatch[E]):String = m.tokens.map(_.c).mkString
  describe("matchesIn") {
    Seq(
      (("ScalA", A1), Some("al"))
    ).foreach{
      case ((string, automaton:Automaton[E]), expected) =>
        val seq = sequence(string)
        it(s"should find $expected in $string") {
          val matches = seqMatch(automaton, seq)
          assert(matches.headOption.map(m => matchToString(m)) == expected)
        }
    }
  }
  describe("allPrefixMatches") {
    describe("without capturing group") {

      Seq(
        (("ScalA", A1), Seq.empty),
        (("alSca", A1), Seq("al")),
        (("ScalA", A2), Seq.empty),
        (("alSca", A2), Seq("alS", "al")),
        (("acccab", A3), Seq("accc", "acc", "ac")),
        (("aecobc", A4), Seq("aeco")),
        (("aecobc", A5), Seq("aeco", "ae")),
      ).zipWithIndex.foreach{
        case (((string, automaton:Automaton[E]), expected), i) =>
          val seq = sequence(string)
          it(s"${1+i}. should find $expected in $string with automaton $automaton") {
            val matches = allPrefixMatches(automaton, seq)
            val actual = matches.map(m => matchToString(m))
            assert(actual == expected)
          }
      }
    }

    describe("with capturing group") {
      Seq(
        (("aecobc", A6), Seq(("aeco", Map("toto" -> List("ec"))), ("ae", Map.empty))),
      ).zipWithIndex.foreach{
        case (((string, automaton:Automaton[E]), expList), i) =>
          val seq = sequence(string)
          val matches = allPrefixMatches(automaton, seq)
          it(s"${1+i}. should find ${expList.map(_._1)} in $string with automaton $automaton") {
            val actual = matches.map(m => matchToString(m))
            val expTokens = expList.map(_._1)
            assert(actual == expTokens)
          }
          expList.zip(matches).zipWithIndex.foreach{
            case (((tokens, groups), m), j) =>
              it(s"${i+1}.${j+1}. should match $tokens with capturing groups $groups") {
                assert(m.groups.map{case (name,list) => (name, list.map(_.map(_.c).mkString))} == groups)
              }
          }
      }
    }
  }



}
