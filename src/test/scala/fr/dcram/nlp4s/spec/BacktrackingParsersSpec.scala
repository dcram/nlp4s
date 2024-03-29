package fr.dcram.nlp4s.spec

import fr.dcram.nlp4s.parse.BacktrackingParserTypes.Parser
import fr.dcram.nlp4s.parse.BacktrackingParsers
import org.scalatest.funspec.AnyFunSpec

import java.util.concurrent.atomic.AtomicInteger

class BacktrackingParsersSpec extends AnyFunSpec {

  describe(classOf[BacktrackingParsers[Char]].toString) {

    object CharReference extends BacktrackingParsers[Char]
    import CharReference._

    def set(str:String):Parser[Char, Char] = {
      val s = Set(str.toCharArray.toSeq:_*)
      tok(s.contains)
    }
    val vowel = set("aeiouy")
    val parserA:Parser[Char, Char] = tok(_ == 'a')
    val parserB:Parser[Char, Char] = tok(_ == 'b')

    describe("parse") {
        describe("prepare") {
          Seq(
            (parserA, "a", Some('a'), List('a')),
            (parserA, "A", None, List.empty[Char]),
            (parserA.endoPrepare(_.toLower), "a", Some('a'), List('a')),
            (parserA.endoPrepare(_.toLower), "A", Some('a'), List('A')),
          ).zipWithIndex.foreach {
            case ((parser, str, m, tokens), i) =>
              it(s"${i}a. should extract $m from sequence $str") {assert(parser.parse(str).map(_.data) == m)}
              it(s"${i}b. matched tokens in sequence $str should be ${tokens}") {assert(parser.parse(str).map(_.tokens).getOrElse(List.empty) == tokens)}
          }

        }

        describe("filter") {
          val digit:Parser[Char, Int] = tok(_.isDigit).map(_.toString.toInt)
          val digitEven:Parser[Char, Int] = digit.filter(_ % 2 == 0)
          Seq(
            (digit, "a", None),
            (digit, "1", Some(1)),
            (digit, "2", Some(2)),
            (digitEven, "a", None),
            (digitEven, "1", None),
            (digitEven, "2", Some(2)),
          ).zipWithIndex.foreach {
            case ((parser, str, m), i) =>
              it(s"$i. should extract $m from sequence $str") {assert(parser.parse(str).map(_.data) == m)}
          }

        }

        describe("laziness") {
        describe("a | b") {
          val aCnt = new AtomicInteger(0)
          val bCnt = new AtomicInteger(0)
          val a:Parser[Char, Char] = tok{c => aCnt.incrementAndGet(); c == 'a'}
          val b:Parser[Char, Char] = tok{c => bCnt.incrementAndGet(); c == 'b'}

          it("result should be 'a'") {assert((a or b).parse("a").map(_.data) == Some('a'))}
          it("parser a should have been invoked once") {assert(aCnt.intValue() == 1)}
          it("parser b should have been invoked zero time") {assert(bCnt.intValue() == 0)}
        }
      }


      describe("operators") {
        Seq(
          ("abc", parserA, Some('a')),
          ("abc", parserA.map(_.toUpper), Some('A')),
          ("abc", parserB, None),
          ("abc", parserB or parserA.map(_.toUpper), Some('A')),
          ("abc", (parserA seq parserB).map{case (x,y) => s"$x$y"}.map(_.toUpperCase), Some("AB")),
          ("abc", (parserB seq parserA).map{case (x,y) => s"$x$y"}.map(_.toUpperCase), None),
          ("aebc", (vowel seq vowel seq parserB).map{case ((x,y),z) => s"$x$y$z"}, Some("aeb")),
          ("aebc", vowel.rep(0).map(_.mkString("")), Some("")),
          ("aebc", vowel.rep(1).map(_.mkString("")), Some("a")),
          ("aebc", vowel.rep(2).map(_.mkString("")), Some("ae")),
          ("aebc", vowel.rep(3).map(_.mkString("")), None),
          ("aeiyuibc", vowel.opt, Some(Some('a'))),
          ("beiyuibc", vowel.opt, Some(None)),
          ("aeiyuibc", vowel.*().map(_.mkString), Some("aeiyui")),
          ("aeb", (vowel.*() ~ vowel ~ parserB).map{case ((x,y),z) => s"${x.mkString}$y$z"}, Some("aeb")),
          ("ab", (vowel.opt seq vowel seq parserB).map{case ((x,y),z) => s"${x.mkString}$y$z"}, Some("ab")),
          ("a", (vowel.opt seq vowel).map{case (x,y) => s"${x.mkString}$y"}, Some("a")),
        ).zipWithIndex.foreach{
          case ((string, parser, result), i) =>
            it(s"$i. should extract $result from string $string") {
              assert(parser.parse(string.toCharArray.toSeq).map(_.data) == result)
            }
        }
      }


    }



  }

}
