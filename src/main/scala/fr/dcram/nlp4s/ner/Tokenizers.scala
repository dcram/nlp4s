package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.util.Monoid

trait Tokenizers extends Monoid[StringTokenizer] {
  self =>

  override def zero:StringTokenizer = str => Stream(Token(0, str.length, str))
  override def op(t1: StringTokenizer, t2:StringTokenizer):StringTokenizer = str => t1(str).flatMap(tok => t2(tok.obj).map{
      case Token(begin2, end2, str) => Token(tok.begin + begin2, tok.begin + end2, str)
    })

  implicit def tokenizerOps(t:StringTokenizer):TokenizerOps = TokenizerOps(t)

  case class TokenizerOps(t:StringTokenizer) {
    def chain(t2:StringTokenizer):StringTokenizer = self.op(t, t2)
  }
}
