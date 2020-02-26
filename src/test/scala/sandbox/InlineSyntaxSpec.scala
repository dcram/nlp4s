package sandbox

import fr.dcram.InProgress
import org.scalatest.FunSpec

class InlineSyntaxSpec extends FunSpec {

  case class LabelAutomaton(label:String)  {
    def /(o:LabelAutomaton): LabelAutomaton = LabelAutomaton(s"($label)|(${o.label})")
    def *(): LabelAutomaton = LabelAutomaton(s"($label)*")
    def +(): LabelAutomaton  = LabelAutomaton(s"($label)+")
    def ?(): LabelAutomaton = LabelAutomaton(s"($label)?")
    def ~>(o:LabelAutomaton): LabelAutomaton = LabelAutomaton(s"($label) (${o.label})")
    def ~(n:Int): LabelAutomaton = LabelAutomaton(s"($label){$n}")
    def ~(m:Int,n:Int): LabelAutomaton = LabelAutomaton(s"($label){$m,$n}")
  }

  object A extends LabelAutomaton("a")
  object B extends LabelAutomaton("b")
  object C extends LabelAutomaton("c")
  object D extends LabelAutomaton("d")

  describe("inline syntax") {
    Seq(
      (1, A / B, "(a)|(b)"),
      (2, (A / B)*, "((a)|(b))*"),
      (3, A / B.*, "(a)|((b)*)"),
      (4, A~(3,4) / B.*, "((a){3,4})|((b)*)"),
      (5, A ~> B ~> C, "((a) (b)) (c)"),
      (6, A ~> B / C ~> D, "(((a) (b))|(c)) (d)"),
      (7, A ~> (B / C) ~> D, "((a) ((b)|(c))) (d)"),
      (8, A ~> B / C, "((a) (b))|(c)"),
      (9, A ~> B / C ~> D, "(((a) (b))|(c)) (d)"),
      (10, A / B ~> C / D, ""),
    ).foreach{
      case (i, a, label) => it(s"$i. should labelize ${a.label} as $label", InProgress){assert(a.label == label)}
    }
  }
}
