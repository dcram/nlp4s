package fr.dcram.nlp4s.automata

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.FunSpec

class TokParserSpec extends FunSpec {

  describe(classOf[AutomataReference[Char]].toString) {

    object CharReference extends AutomataReference[Char]
    import CharReference._

    def set(str:String):AutomataReferenceTypes.Parser[Char, Char] = {
      val s = Set(str.toCharArray.toSeq:_*)
      tok(s.contains)
    }
    val vowel = set("aeiouy")
    val parserA:AutomataReferenceTypes.Parser[Char, Char] = tok(_ == 'a')
    val parserB:AutomataReferenceTypes.Parser[Char, Char] = tok(_ == 'b')

    describe("parse") {
      describe("laziness") {
        describe("a | b") {
          val aCnt = new AtomicInteger(0)
          val bCnt = new AtomicInteger(0)
          val a:AutomataReferenceTypes.Parser[Char, Char] = tok{case c => aCnt.incrementAndGet(); c == 'a'}
          val b:AutomataReferenceTypes.Parser[Char, Char] = tok{case c => bCnt.incrementAndGet(); c == 'b'}

          it("result should be 'a'") {assert((a or b).parse("a") == Some('a'))}
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
          ("abc", (parserA ~ parserB).map{case (x,y) => s"$x$y"}.map(_.toUpperCase), Some("AB")),
          ("abc", (parserB ~ parserA).map{case (x,y) => s"$x$y"}.map(_.toUpperCase), None),
          ("aebc", ((vowel ~ vowel ~ parserB)).map{case ((x,y),z) => s"$x$y$z"}, Some("aeb")),
          ("aebc", vowel.rep(0).map(_.mkString("")), Some("")),
          ("aebc", vowel.rep(1).map(_.mkString("")), Some("a")),
          ("aebc", vowel.rep(2).map(_.mkString("")), Some("ae")),
          ("aebc", vowel.rep(3).map(_.mkString("")), None),
          ("aeiyuibc", vowel.opt, Some(Some('a'))),
          ("beiyuibc", vowel.opt, Some(None)),
          ("aeiyuibc", vowel.*.map(_.mkString), Some("aeiyui")),
          ("aeb", (vowel.* ~ vowel ~ parserB).map{case ((x,y),z) => s"${x.mkString}$y$z"}, Some("aeb")),
          ("ab", (vowel.opt ~ vowel ~ parserB).map{case ((x,y),z) => s"${x.mkString}$y$z"}, Some("ab")),
          ("a", (vowel.opt ~ vowel).map{case (x,y) => s"${x.mkString}$y"}, Some("a")),
        ).zipWithIndex.foreach{
          case ((string, parser, result), i) =>
            it(s"$i. should extract $result from string $string") {
              assert(parser.parse(string.toCharArray.toSeq) == result)
            }
        }
      }


    }



  }

}
