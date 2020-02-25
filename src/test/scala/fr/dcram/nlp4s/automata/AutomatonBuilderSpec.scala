package fr.dcram.nlp4s.automata

import org.scalatest.FunSpec

class AutomatonBuilderSpec extends FunSpec {
  import AutomataTests._
  import AutomatonFactory._

  describe(AutomatonFactory.toString) {
    def testMatching(seqString:String, aut:Automaton[E], matches: Seq[String]):Unit = it(s"should extract matches $matches when matching $aut on sequence $seqString") {
      assert(seqMatch(aut, fixSeq(seqString)).map(_.tokens.collect{case UserToken(E(c)) => c}.mkString) == matches)
    }

    describe("preambule") {
      Seq(
        ("aba", AutA, Seq("a", "a")),
        ("aba", AutB, Seq("b")),
        ("aba", AutC, Seq.empty),
      ).foreach {
        case (s, aut, matches) => testMatching(s, aut, matches)
      }
    }
    describe("append") {
      Seq(
        ("aba", sequence(Letter('a'), AutB), Seq("ab")),
        ("abacabac", sequence(AutC, AutA, AutB, AutA), Seq("caba")),
        ("abacabac", sequence(AutB, AutB), Seq.empty),
        ("abacabac", sequence(AutA, Consomn), Seq("ab", "ac", "ab", "ac")),
      ).foreach {
        case (s, aut, matches) => testMatching(s, aut, matches)
      }
    }
    describe("or") {
      Seq(
        ("a", or(Letter('a'), Letter('b')), Seq("a")),
        ("b", or(Letter('a'), Letter('b')), Seq("b")),
        ("c", or(Letter('a'), Letter('b')), Seq()),
        ("eafgbh", sequence(Vowel, or(Letter('a'), Letter('b'))), Seq("ea")),
        ("eafgcatbubh", sequence(Vowel, or(Letter('a'), Letter('b'))), Seq("ea", "ub")),
      ).foreach {
        case (s, aut, matches) => testMatching(s, aut, matches)
      }
    }
    describe("zeroOne") {
      Seq(
        ("a", zeroOne(Letter('b')), Seq("")),
        ("b", zeroOne(Letter('b')), Seq("b", "")),
        ("bbb", zeroOne(Letter('b')), Seq("b", "b", "b", "")),
      ).foreach {
        case (s, aut, matches) => testMatching(s, aut, matches)
      }
    }
  }

}
