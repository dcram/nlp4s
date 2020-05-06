package fr.dcram.nlp4s.automata

object AutomataReferenceTypes {
  type Parser[Tok, +A] = Seq[Tok] => Stream[Result[Tok, A]]
  case class Result[+Tok, +A](seq:Seq[Tok], m:A)
}

import fr.dcram.nlp4s.automata.AutomataReferenceTypes._
trait AutomataReference[Tok] extends Parsers[Tok, Parser] {

  override def succeed[A](a: A): Parser[Tok, A] = seq => Stream(Result(seq, a))

  override def fromOpt[A](f: Tok => Option[A]): Parser[Tok, A] = seq => seq match {
    case tok +: tail  =>
      f(tok) match {
        case Some(a)  => Stream(Result(tail, a))
        case None     => Stream.empty
      }
    case _ => Stream.empty
  }

  override def flatMap[A, B](p: Parser[Tok, A])(f: A => Parser[Tok, B]): Parser[Tok, B] = seq => {
    p(seq).foldLeft(Stream.empty[Result[Tok, B]]){case (acc, Result(tail, a)) => acc #::: f(a)(tail)}
  }

  override def map[A, B](p: Parser[Tok, A])(f: A => B): Parser[Tok, B] = seq => {
    p(seq).map{case Result(tail, a) => Result(tail, f(a))}
  }

  override def or[A, B >: A](p1: Parser[Tok, A], p2: => Parser[Tok, B]): Parser[Tok, B] = seq => p1(seq) #::: p2(seq)

  override def ~[A, B](p1: Parser[Tok, A], p2: => Parser[Tok, B]): Parser[Tok, (A, B)] = seq =>
    p1(seq).foldLeft(Stream.empty[Result[Tok, (A,B)]]){case (acc, Result(tail, a)) => acc #::: p2.map(b => (a,b))(tail)}

  override def parse[A](p: Parser[Tok, A])(seq: Seq[Tok]): Option[A] = p(seq).headOption.map(_.m)

  override def scan[A](p:Parser[Tok, A])(seq:Seq[Tok]):Stream[A] = {
    p(seq).headOption match {
      case Some(Result(tail, a)) =>
        a #:: scan(p)(tail)
      case None =>
        seq match {
          case _ +: tail => scan(p)(tail)
          case Nil => Stream.empty
        }
    }
  }


  def seq[A1,A2](p1:Parser[Tok, A1], p2: Parser[Tok, A2]):Parser[Tok, (A1,A2)] = p1 ~ p2
  def seq[A1,A2,A3](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3]):Parser[Tok, (A1,A2,A3)] = (p1 ~ p2 ~ p3).map{case ((a1,a2),a3) => (a1, a2, a3)}
  def seq[A1,A2,A3,A4](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4]):Parser[Tok, (A1,A2,A3,A4)] = (p1 ~ p2 ~ p3 ~ p4).map{case (((a1,a2),a3),a4) => (a1, a2, a3, a4)}
  def seq[A1,A2,A3,A4,A5](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4], p5: Parser[Tok, A5]):Parser[Tok, (A1,A2,A3,A4,A5)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5).map{case ((((a1,a2),a3),a4),a5) => (a1, a2, a3, a4, a5)}
  def seq[A1,A2,A3,A4,A5,A6](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4], p5: Parser[Tok, A5], p6: Parser[Tok, A6]):Parser[Tok, (A1,A2,A3,A4,A5,A6)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6).map{case (((((a1,a2),a3),a4),a5),a6) => (a1, a2, a3, a4, a5, a6)}
  def seq[A1,A2,A3,A4,A5,A6,A7](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4], p5: Parser[Tok, A5], p6: Parser[Tok, A6], p7: Parser[Tok, A7]):Parser[Tok, (A1,A2,A3,A4,A5,A6,A7)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7).map{case ((((((a1,a2),a3),a4),a5),a6),a7) => (a1, a2, a3, a4, a5, a6, a7)}
  def seq[A1,A2,A3,A4,A5,A6,A7,A8](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4], p5: Parser[Tok, A5], p6: Parser[Tok, A6], p7: Parser[Tok, A7], p8: Parser[Tok, A8]):Parser[Tok, (A1,A2,A3,A4,A5,A6,A7,A8)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8).map{case (((((((a1,a2),a3),a4),a5),a6),a7),a8) => (a1, a2, a3, a4, a5, a6, a7, a8)}
  def seq[A1,A2,A3,A4,A5,A6,A7,A8,A9](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4], p5: Parser[Tok, A5], p6: Parser[Tok, A6], p7: Parser[Tok, A7], p8: Parser[Tok, A8], p9: Parser[Tok, A9]):Parser[Tok, (A1,A2,A3,A4,A5,A6,A7,A8,A9)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8 ~ p9).map{case ((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9) => (a1, a2, a3, a4, a5, a6, a7, a8, a9)}
  def seq[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](p1:Parser[Tok, A1], p2: Parser[Tok, A2], p3: Parser[Tok, A3], p4: Parser[Tok, A4], p5: Parser[Tok, A5], p6: Parser[Tok, A6], p7: Parser[Tok, A7], p8: Parser[Tok, A8], p9: Parser[Tok, A9], p10: Parser[Tok, A10]):Parser[Tok, (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8 ~ p9 ~ p10).map{case (((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9),a10) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)}

  def or[A](p1:Parser[Tok, A], p2: => Parser[Tok, A], p3: => Parser[Tok, A]):Parser[Tok, A] = p1 or p2 or p3
  def or[A](p1:Parser[Tok, A], p2: => Parser[Tok, A], p3: => Parser[Tok, A], p4: => Parser[Tok, A]):Parser[Tok, A] = p1 or p2 or p3 or p4
  def or[A](p1:Parser[Tok, A], p2: => Parser[Tok, A], p3: => Parser[Tok, A], p4: => Parser[Tok, A], p5: => Parser[Tok, A]):Parser[Tok, A] = p1 or p2 or p3 or p4 or p5
  def or[A](p1:Parser[Tok, A], p2: => Parser[Tok, A], p3: => Parser[Tok, A], p4: => Parser[Tok, A], p5: => Parser[Tok, A], p6: => Parser[Tok, A]):Parser[Tok, A] = p1 or p2 or p3 or p4 or p5 or p6
  def or[A](p1:Parser[Tok, A], p2: => Parser[Tok, A], p3: => Parser[Tok, A], p4: => Parser[Tok, A], p5: => Parser[Tok, A], p6: => Parser[Tok, A], p7: => Parser[Tok, A]):Parser[Tok, A] = p1 or p2 or p3 or p4 or p5 or p6 or p7
  def or[A](p1:Parser[Tok, A], p2: => Parser[Tok, A], p3: => Parser[Tok, A], p4: => Parser[Tok, A], p5: => Parser[Tok, A], p6: => Parser[Tok, A], p7: => Parser[Tok, A], p8: => Parser[Tok, A]):Parser[Tok, A] = p1 or p2 or p3 or p4 or p5 or p6 or p7 or p8

}
