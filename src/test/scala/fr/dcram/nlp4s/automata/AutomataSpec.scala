package fr.dcram.nlp4s.automata

import fr.dcram.nlp4s.automata.AutomataTests.{AutVowCons, E, fixSeq}
import org.scalatest.FunSpec

class AutomataSpec extends FunSpec {
  import AutomataTests._

  describe("matchesIn") {
    Seq(
      (("ScalA", AutVowCons), Some("al"))
    ).foreach{
      case ((string, automaton:Automaton[E]), expected) =>
        val seq = fixSeq(string)
        it(s"should find $expected in $string") {
          val matches = seqMatch(automaton, seq)
          assert(matches.headOption.map(m => matchToString(m)) == expected)
        }
    }
  }

  describe("seqMatch") {
    def doTest(s:String, aut:Automaton[E], matches:Seq[String]) = it(s"should extract matches $matches when matching $aut on sequence $s") {
      assert(seqMatch(aut, fixSeq(s)).map(_.tokens.collect{case UserToken(E(c)) => c}.mkString) == matches)
    }
    describe("termination") {
      Seq(
        ("", ZeroOneA, Seq("")),
        ("aa", ZeroOneA, Seq("a", "a", "")),
        ("b", ZeroOneA, Seq("", "")),
      ).foreach {
        case (s, aut, matches) => doTest(s, aut, matches)
      }
    }

    describe("non overlapping") {
      Seq(
        ("aba", AutAConsA, Seq("aba")),
        ("abacadafa", AutAConsA, Seq("aba", "ada")),
      ).foreach {
        case (s, aut, matches) => doTest(s, aut, matches)
      }
    }
  }

  describe("allPrefixMatches") {
    describe("without capturing group") {

      Seq(
        (("ScalA", AutVowCons), Seq.empty),
        (("alSca", AutVowCons), Seq("al")),
        (("ScalA", AutVowConsCons), Seq.empty),
        (("alSca", AutVowConsCons), Seq("alS", "al")),
        (("acccab", AutVowConsPlus), Seq("accc", "acc", "ac")),
        (("aecobc", A4), Seq("aeco")),
        (("aecobc", A5), Seq("aeco", "ae")),
      ).zipWithIndex.foreach{
        case (((string, automaton:Automaton[E]), expected), i) =>
          val seq = fixSeq(string)
          val matches = allPrefixMatches(automaton, seq)
          it(s"${1+i}. should find $expected in $string with automaton $automaton") {
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
          val seq = fixSeq(string)
          val matches = allPrefixMatches(automaton, seq)
          it(s"${1+i}. should find ${expList.map(_._1)} in $string with automaton $automaton") {
            val actual = matches.map(m => matchToString(m))
            val expTokens = expList.map(_._1)
            assert(actual == expTokens)
          }
          expList.zip(matches).zipWithIndex.foreach{
            case (((tokens, groups), m), j) =>
              it(s"${i+1}.${j+1}. should match $tokens with capturing groups $groups") {
                assert(m.groups.map{case (name,list) => (name, list.map(_.collect{case UserToken(E(c)) => c}.mkString))} == groups)
              }
          }
      }
    }
  }



}
