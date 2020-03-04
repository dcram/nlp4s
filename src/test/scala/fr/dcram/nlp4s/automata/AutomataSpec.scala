package fr.dcram.nlp4s.automata

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
    def doTest(s:String, aut:Automaton[E], matches:Seq[String]):Unit = it(s"should extract matches $matches when matching $aut on sequence $s") {
      assert(seqMatch(aut, fixSeq(s)).map(_.tokens.map(_.c).mkString) == matches)
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
          it(s"${1+i}. should find $expected in $string with automaton $automaton") {
            val matches = allPrefixMatches(automaton, seq)
            val actual = matches.map(m => matchToString(m))
            assert(actual == expected)
          }
      }
    }

    describe("with capturing group") {
      import AutomatonFactory._
      Seq(
        (
          "aecobc",
          A6,
          Seq(
            ("aeco", Map("toto" -> Seq(("ec", Map.empty)))),
            ("ae", Map.empty))),
        (
          "aa",
          sequence(star(Letter('a')), Letter('a')),
          Seq(
            ("aa", Map.empty),
            ("a", Map.empty)),
        ),
        (
          "aa",
          sequence(named("as", star(Letter('a'))), Letter('a')),
          Seq(
            ("aa", Map("as" -> Seq(("a", Map.empty)))),
            ("a", Map("as" -> Seq(("", Map.empty)))),
          )),
        (
          "abacbcdy",
          sequence(Letter('a'), named("toto", Letter('b'), Letter('a'), Letter('c'))),
          Seq(
            ("abac", Map("toto" -> Seq(("bac", Map.empty))))
          )),
        (
          "abacbcdy",
          A7,
          Seq(
            ("abacbcd", Map("toto" -> Seq(("bacbcd", Map("tata" -> Seq(("bac", Map.empty), ("bc", Map.empty))))))))),
      ).zipWithIndex.foreach{
        case ((string, automaton:Automaton[E], expMatches), i) =>
          val seq = fixSeq(string)

          val actualMatches = allPrefixMatches(automaton, seq)

          def toStr(m:RegexMatch[E]):(String, Map[String, Any]) = (
            matchToString(m),
            m.groups.map{case (name, gm) => (name, gm.map(mm => toStr(mm)))}
          )
          val actualMatchesStr = actualMatches.map(toStr)

          it(s"${1+i}. should find ${expMatches.map(_._1)} in $string with automaton $automaton") {
            val actual = actualMatches.map(m => matchToString(m))
            val expTokens = expMatches.map(_._1)
            assert(actual == expTokens)
          }
          actualMatchesStr.zip(expMatches).foreach {
            case (actual, expected) =>
              it(s"${1+i}. Match $actual should eq $expected") {
                assert(actual == expected)
              }
          }
      }
    }
  }
}
