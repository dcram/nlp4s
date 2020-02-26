package fr.dcram.nlp4s.automata

import org.scalatest.FunSpec

class AutomatonFactorySpec extends FunSpec {
  import AutomataTests._
  import AutomatonFactory._

  describe(AutomatonFactory.toString) {
    def testMatching(index:Int, seqString:String, aut:Automaton[E], matches: Seq[String]):Unit = it(s"$index. should extract matches $matches when matching $aut on sequence $seqString") {
      assert(seqMatch(aut, fixSeq(seqString)).map(_.tokens.collect{case UserToken(E(c)) => c}.mkString) == matches)
    }

    def testAll(tests:(String, Automaton[E], Seq[String])*):Unit = {
      tests.zipWithIndex.foreach{case ((s, aut, matches), i) => testMatching(1+i, s, aut, matches)}
    }
    describe("preambule") {
      testAll(
        ("aba", AutA, Seq("a", "a")),
        ("aba", AutB, Seq("b")),
        ("aba", AutC, Seq.empty),
      )
    }
    describe("append") {
      testAll(
        ("aba", sequence(Letter('a'), AutB), Seq("ab")),
        ("abacabac", sequence(AutC, AutA, AutB, AutA), Seq("caba")),
        ("abacabac", sequence(AutB, AutB), Seq.empty),
        ("abacabac", sequence(AutA, Consomn), Seq("ab", "ac", "ab", "ac")),
      )
    }
    describe("or") {
      testAll(
        ("a", or(Letter('a'), Letter('b')), Seq("a")),
        ("b", or(Letter('a'), Letter('b')), Seq("b")),
        ("c", or(Letter('a'), Letter('b')), Seq()),
        ("eafgbh", sequence(Vowel, or(Letter('a'), Letter('b'))), Seq("ea")),
        ("eafgcatbubh", sequence(Vowel, or(Letter('a'), Letter('b'))), Seq("ea", "ub")),
      )
    }
    describe("zeroOne") {
      testAll(
        ("a", zeroOne(Letter('b')), Seq("", "")),
        ("b", zeroOne(Letter('b')), Seq("b", "")),
        ("bbb", zeroOne(Letter('b')), Seq("b", "b", "b", "")),
      )
    }
    describe("star") {
      testAll(
        ("a", star(Letter('b')), Seq("", "")),
        ("b", star(Letter('b')), Seq("b", "")),
        ("bbb", star(Letter('b')), Seq("bbb", "")),
      )
    }
    describe("quantified") {
      import Quantifiers._
      describe("plus") {
        testAll(
          ("a", quantified(Letter('b'), Plus), Seq.empty),
          ("abab", quantified(Letter('b'), Plus), Seq("b", "b")),
          ("bbb", quantified(Letter('b'), Plus), Seq("bbb")),
        )
      }

      describe("n") {
        testAll(
          ("a", quantified(Letter('b'), N(2)), Seq.empty),
          ("abab", quantified(Letter('b'), N(1)), Seq("b", "b")),
          ("abab", quantified(Letter('b'), N(2)), Seq.empty),
          ("abbab", quantified(Letter('b'), N(2)), Seq("bb")),
          ("abbbab", quantified(Letter('b'), N(2)), Seq("bb")),
        )
      }

      describe("zeron") {
        testAll(
          ("a", quantified(Letter('b'), ZeroN(1)), Seq("", "")),
          ("b", quantified(Letter('b'), ZeroN(1)), Seq("b", "")),
          ("bbb", quantified(Letter('b'), ZeroN(1)), Seq("b", "b", "b", "")),
          ("a", quantified(Letter('b'), ZeroN(2)), Seq("", "")),
          ("b", quantified(Letter('b'), ZeroN(2)), Seq("b", "")),
          ("bb", quantified(Letter('b'), ZeroN(2)), Seq("bb", "")),
          ("bbb", quantified(Letter('b'), ZeroN(2)), Seq("bb", "b", "")),
        )
      }

      describe("mn") {
        testAll(
          ("a", quantified(Letter('b'), MN(1,2)), Seq.empty),
          ("abc", quantified(Letter('b'), MN(1,2)), Seq("b")),
          ("abbc", quantified(Letter('b'), MN(1,2)), Seq("bb")),
          ("abbbc", quantified(Letter('b'), MN(1,2)), Seq("bb", "b")),
          ("abbbbc", quantified(Letter('b'), MN(1,2)), Seq("bb", "bb")),
          ("abbbbbc1", quantified(Letter('b'), MN(1,2)), Seq("bb", "bb", "b")),
          ("abbbbbc2", quantified(Letter('b'), MN(2,3)), Seq("bbb", "bb")),
        )
      }

      describe("nstar") {
        testAll(
          ("a", quantified(Letter('b'), NStar(0)), Seq("", "")),
          ("b", quantified(Letter('b'), NStar(0)), Seq("b", "")),
          ("bbb", quantified(Letter('b'), NStar(0)), Seq("bbb", "")),
          ("a", quantified(Letter('b'), NStar(1)), Seq.empty),
          ("abab", quantified(Letter('b'), NStar(1)), Seq("b", "b")),
          ("bbb", quantified(Letter('b'), NStar(1)), Seq("bbb")),
          ("a", quantified(Letter('b'), NStar(2)), Seq.empty),
          ("abab", quantified(Letter('b'), NStar(2)), Seq.empty),
          ("bbb", quantified(Letter('b'), NStar(2)), Seq("bbb")),
        )

      }

    }

  }

}
