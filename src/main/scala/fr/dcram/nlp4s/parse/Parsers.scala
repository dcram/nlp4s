package fr.dcram.nlp4s.parse

import fr.dcram.nlp4s.parse.ParserTypes.{Parser, Result}

trait Parsers[Tok] extends ParsersAutomataAlgebra[({type f[+x] = Parser[Tok, x]})#f] {
  self =>

  override def succeed[A](a: A): Parser[Tok, A] = seq => Stream(Result(seq, a))

  override def flatMap[A, B](p: Parser[Tok, A])(f: A => Parser[Tok, B]): Parser[Tok, B] = seq => {
    p(seq).foldLeft(Stream.empty[Result[Tok, B]]) { case (acc, Result(tail, a)) => acc #::: f(a)(tail) }
  }

  override def map[A, B](p: Parser[Tok, A])(f: A => B): Parser[Tok, B] = seq => {
    p(seq).map { case Result(tail, a) => Result(tail, f(a)) }
  }

  override def or[A, B >: A](p1: Parser[Tok, A], p2: => Parser[Tok, B]): Parser[Tok, B] = seq => p1(seq) #::: p2(seq)

  override def map2[A, B](p1: Parser[Tok, A], p2: => Parser[Tok, B]): Parser[Tok, (A, B)] = seq =>
    p1(seq).foldLeft(Stream.empty[Result[Tok, (A, B)]]) { case (acc, Result(tail, a)) => acc #::: p2.map(b => (a, b))(tail) }


  def tok(f: Tok => Boolean): Parser[Tok, Tok] = fromOpt(tok => if (f(tok)) Some(tok) else None)

  def fromOpt[A](f: Tok => Option[A]): Parser[Tok, A] = seq => seq match {
    case tok +: tail =>
      f(tok) match {
        case Some(a) => Stream(Result(tail, a))
        case None => Stream.empty
      }
    case _ => Stream.empty
  }

  def filter[A](p: Parser[Tok, A])(f: A => Boolean): Parser[Tok, A] = seq => p(seq).filter(r => f(r.m))

  def parse[A](p: Parser[Tok, A])(seq: Seq[Tok]): Option[A] = p(seq).headOption.map(_.m)

  def scan[A](p: Parser[Tok, A])(seq: Seq[Tok]): Stream[A] = {
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

  implicit def parserOps[A](p: Parser[Tok, A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[Tok, A]) {
    def filter(f: A => Boolean): Parser[Tok, A] = self.filter(p)(f)
    def parse(seq: Seq[Tok]): Option[A] = self.parse(p)(seq)
    def scan(seq: Seq[Tok]): Stream[A] = self.scan(p)(seq)
  }
}