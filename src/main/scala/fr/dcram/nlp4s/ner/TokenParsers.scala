package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.parse.Parsers
import fr.dcram.nlp4s.ner.NerTypes.TokenParser
import fr.dcram.nlp4s.ner.Token.MergeApplicative
import fr.dcram.nlp4s.parse.ParserTypes.Result
import fr.dcram.nlp4s.util.Trie

import scala.util.matching.Regex


trait TokenParsers extends Parsers[Token[String]] with SeqArities {
  ref =>

  def digit:TokenParser[String] = reg("""^\d+$""".r).map(_.group(0))
  def digit(n:Int):TokenParser[String] = reg(s"^\\d{$n}$$".r).map(_.group(0))
  def fromOptTok[B](f:String => Option[B]):TokenParser[B] = fromOpt(t => f(t.obj).map(b => t.copy(obj = b)))
  def %[B](f:String => Option[B]):TokenParser[B] = fromOptTok(f)
  def ###[B](f:String => Boolean):TokenParser[String] = fromOptTok(str => if(f(str)) Some(str) else None)

  def inSet(strings:Set[String]):TokenParser[String] = ###(strings.contains)
  def inMap[V](map:Map[String, V]):TokenParser[V] = fromOptTok(map.get)

  def inTrie[V](trie:Trie[String, V]):TokenParser[(V, List[String])] = {
    def doInTrie(trie:Trie[String, V], tokens:List[Token[String]]): TokenParser[(V, List[String])] = or(
      s => s match {
        case tok +: tail => trie.getChild(tok.obj) match {
          case Some(child) => doInTrie(child, tok :: tokens)(tail)
          case None => Stream.empty
        }
        case _ => Stream.empty
      },
      s => trie.value match {
        case Some(v) =>
          val tok = MergeApplicative.sequence(tokens.reverse).map(tokens => (v,tokens))
          Stream(Result(s, tok))
        case None => Stream.empty
      }
    )

    doInTrie(trie, List.empty)

  }

  implicit def reg(r:Regex):TokenParser[Regex.Match] = fromOptTok(r.findFirstMatchIn)
  implicit def str(str:String):TokenParser[String] = fromOptTok(t => if(t == str) Some(str) else None)
  implicit def ops[A1,A](p:A1)(implicit f: A1 => TokenParser[A]):TokenParserOps[A] = TokenParserOps(p)
  implicit def ops2[A](p:TokenParser[A]):TokenParserOps[A] = TokenParserOps(p)

  case class TokenParserOps[A](p:TokenParser[A]) {
    private[this] val TokenApp = Token.MergeApplicative
    def mn(m:Int, n:Int):TokenParser[List[A]] = ref.map(ref.mn(m,n)(p))(TokenApp.sequence)
    def n(n:Int):TokenParser[List[A]] = ref.map(ref.rep(n)(p))(TokenApp.sequence)
    def atLeastN(n:Int):TokenParser[List[A]] = ref.map(ref.atLeastN(n)(p))(TokenApp.sequence)
    def atMostN(n:Int):TokenParser[List[A]] = ref.map(ref.atMostN(n)(p))(TokenApp.sequence)
    def +():TokenParser[List[A]] = ref.map(ref.plus(p))(TokenApp.sequence)
    def *():TokenParser[List[A]] = ref.map(ref.star(p))(TokenApp.sequence)
    def opt:TokenParser[Option[A]] = ref.map(ref.opt(p))(TokenApp.option)
    def scan(seq:Seq[Token[String]]):Stream[Token[A]] = ref.scan(p)(seq)
    def map[B](f: A => B): TokenParser[B] = ref.map(p)(_.map(f))
    def |[B>:A](p2:TokenParser[B]): TokenParser[B] = ref.or(p,p2)
    def ~[B](p2:TokenParser[B]): TokenParser[(A,B)] = ref.map(ref.map2(p,p2)){case (a,b) => TokenApp.product(a,b)}

  }

}

trait SeqArities {
  this:Parsers[Token[String]] =>
  private[this] val TokenApp = Token.MergeApplicative

  def $[A1](p1:TokenParser[A1]):TokenParser[A1] = p1

  def $[A1,A2](
                p1:TokenParser[A1],
                p2: => TokenParser[A2],
              ):
  TokenParser[(A1,A2)] = seq(p1, p2)
    .map{case (a1,a2) => TokenApp.map2(a1, a2)(Tuple2.apply)}

  def $[A1,A2,A3](
                   p1:TokenParser[A1],
                   p2: => TokenParser[A2],
                   p3: => TokenParser[A3],
                 ):
  TokenParser[(A1,A2,A3)] = seq(p1, p2, p3)
    .map{case (a1,a2,a3) => TokenApp.map3(a1, a2, a3)(Tuple3.apply)}
  def $[A1,A2,A3,A4](
                      p1:TokenParser[A1],
                      p2: => TokenParser[A2],
                      p3: => TokenParser[A3],
                      p4: => TokenParser[A4],
                    ):
  TokenParser[(A1,A2,A3,A4)] = seq(p1, p2, p3, p4)
    .map{case (a1,a2,a3,a4) => TokenApp.map4(a1, a2, a3, a4)(Tuple4.apply)}
  def $[A1,A2,A3,A4,A5](
                         p1:TokenParser[A1],
                         p2: => TokenParser[A2],
                         p3: => TokenParser[A3],
                         p4: => TokenParser[A4],
                         p5: => TokenParser[A5],
                       ):
  TokenParser[(A1,A2,A3,A4,A5)] = seq(p1, p2, p3, p4, p5)
    .map{case (a1,a2,a3,a4,a5) => TokenApp.map5(a1, a2, a3, a4, a5)(Tuple5.apply)}

  def $[A1,A2,A3,A4,A5,A6](
                            p1:TokenParser[A1],
                            p2: => TokenParser[A2],
                            p3: => TokenParser[A3],
                            p4: => TokenParser[A4],
                            p5: => TokenParser[A5],
                            p6: => TokenParser[A6]
                          ):
  TokenParser[(A1,A2,A3,A4,A5,A6)] = seq(p1, p2, p3, p4, p5, p6)
    .map{case (a1,a2,a3,a4,a5,a6) => TokenApp.map6(a1, a2, a3, a4, a5, a6)(Tuple6.apply)}

  def $[A1,A2,A3,A4,A5,A6,A7](
                               p1:TokenParser[A1],
                               p2: => TokenParser[A2],
                               p3: => TokenParser[A3],
                               p4: => TokenParser[A4],
                               p5: => TokenParser[A5],
                               p6: => TokenParser[A6],
                               p7: => TokenParser[A7],
                             ):
  TokenParser[(A1,A2,A3,A4,A5,A6,A7)] = seq(p1, p2, p3, p4, p5, p6, p7)
    .map{case (a1,a2,a3,a4,a5,a6,a7) => TokenApp.map7(a1, a2, a3, a4, a5, a6,a7)(Tuple7.apply)}

  def $[A1,A2,A3,A4,A5,A6,A7,A8](
                                  p1:TokenParser[A1],
                                  p2: => TokenParser[A2],
                                  p3: => TokenParser[A3],
                                  p4: => TokenParser[A4],
                                  p5: => TokenParser[A5],
                                  p6: => TokenParser[A6],
                                  p7: => TokenParser[A7],
                                  p8: => TokenParser[A8],
                                ):
  TokenParser[(A1,A2,A3,A4,A5,A6,A7,A8)] = seq(p1, p2, p3, p4, p5, p6, p7, p8)
    .map{case (a1,a2,a3,a4,a5,a6,a7,a8) => TokenApp.map8(a1, a2, a3, a4, a5, a6,a7,a8)(Tuple8.apply)}

  def $[A1,A2,A3,A4,A5,A6,A7,A8,A9](
                                     p1:TokenParser[A1],
                                     p2: => TokenParser[A2],
                                     p3: => TokenParser[A3],
                                     p4: => TokenParser[A4],
                                     p5: => TokenParser[A5],
                                     p6: => TokenParser[A6],
                                     p7: => TokenParser[A7],
                                     p8: => TokenParser[A8],
                                     p9: => TokenParser[A9],
                                   ):
  TokenParser[(A1,A2,A3,A4,A5,A6,A7,A8,A9)] = seq(p1, p2, p3, p4, p5, p6, p7, p8, p9)
    .map{case (a1,a2,a3,a4,a5,a6,a7,a8,a9) => TokenApp.map9(a1, a2, a3, a4, a5, a6,a7,a8,a9)(Tuple9.apply)}

  def $[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](
                                         p1:TokenParser[A1],
                                         p2: => TokenParser[A2],
                                         p3: => TokenParser[A3],
                                         p4: => TokenParser[A4],
                                         p5: => TokenParser[A5],
                                         p6: => TokenParser[A6],
                                         p7: => TokenParser[A7],
                                         p8: => TokenParser[A8],
                                         p9: => TokenParser[A9],
                                         p10: => TokenParser[A10],
                                       ):
  TokenParser[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)] = seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    .map{case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) => TokenApp.map10(a1, a2, a3, a4, a5, a6,a7,a8,a9,a10)(Tuple10.apply)}

}

