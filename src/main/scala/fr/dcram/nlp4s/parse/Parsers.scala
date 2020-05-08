package fr.dcram.nlp4s.parse

import fr.dcram.nlp4s.parse.ParserTypes.{MatchData, Parser, Result}

trait Parsers[Tok] extends ParsersAutomataAlgebra[({type f[+x] = Parser[Tok, x]})#f] {
  self =>

  override def succeed[A](a: A): Parser[Tok, A] = seq => Stream(Result(seq, MatchData(a, List.empty)))

  override def flatMap[A, B](p: Parser[Tok, A])(f: A => Parser[Tok, B]): Parser[Tok, B] = seq => {
    p(seq)
      .foldLeft(Stream.empty[Result[Tok, B]]) {
        case (acc, Result(tail, MatchData(a, tokens))) =>
          acc #::: f(a)(tail).map(r => r.copy(m = r.m.copy(tokens = tokens ++ r.m.tokens)))
      }
  }

  override def map[A, B](p: Parser[Tok, A])(f: A => B): Parser[Tok, B] = seq => {
    p(seq).map { case Result(tail, MatchData(a, tokens)) => Result(tail, MatchData(f(a), tokens)) }
  }

  override def or[A, B >: A](p1: Parser[Tok, A], p2: => Parser[Tok, B]): Parser[Tok, B] = seq => p1(seq) #::: p2(seq)

  override def map2[A, B](p1: Parser[Tok, A], p2: => Parser[Tok, B]): Parser[Tok, (A, B)] = seq =>
    p1(seq).foldLeft(Stream.empty[Result[Tok, (A, B)]]) {
      case (acc, Result(tail, MatchData(a, tokens))) =>
        acc #::: p2
          .map(b => (a, b))(tail)
          .map(r => r.copy(m = r.m.copy(tokens = tokens ++ r.m.tokens)))
    }


  def tok(f: Tok => Boolean): Parser[Tok, Tok] = fromOpt(tok => if (f(tok)) Some(tok) else None)

  def fromOpt[A](f: Tok => Option[A]): Parser[Tok, A] = seq => seq match {
    case tok +: tail =>
      f(tok) match {
        case Some(a) => Stream(Result(tail, MatchData(a, List(tok))))
        case None => Stream.empty
      }
    case _ => Stream.empty
  }

  def filter[A](p: Parser[Tok, A])(f: A => Boolean): Parser[Tok, A] = seq => p(seq).filter(r => f(r.m.m))


  def zip[A,Tok1](p: Parser[Tok, A]):Parser[(Tok, Tok1), A] = ???


  def mapTok[Tok1, Tok2, A](p: Parser[Tok2, A])(f:Tok1 => Tok2):Parser[Tok1, A] = seq => p(seq map f).map {
    case Result(_, MatchData(m, tokens2)) => {
      val (tokens1, tail1) = seq.splitAt(tokens2.length)
      Result(tail1, MatchData(m, tokens1.toList))
    }
  }

  // contramap
  def prepare[A,Tok1](p: Parser[Tok, A])(f:Tok1 => Tok):Parser[Tok1, A] = mapTok(p)(f)

  def parse[A](p: Parser[Tok, A])(seq: Seq[Tok]): Option[MatchData[Tok, A]] = p(seq).headOption.map(_.m)

  def scan[A](p: Parser[Tok, A])(seq: Seq[Tok]): Stream[MatchData[Tok, A]] = {
    p(seq).headOption match {
      case Some(Result(tail, m)) =>
        m #:: scan(p)(tail)
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
    def parse(seq: Seq[Tok]): Option[MatchData[Tok, A]] = self.parse(p)(seq)
    def scan(seq: Seq[Tok]): Stream[MatchData[Tok, A]] = self.scan(p)(seq)
    def prepare[Tok1](f:Tok1 => Tok): Parser[Tok1, A] = self.prepare(p)(f)
    def endoPrepare(f:Tok => Tok): Parser[Tok, A] = prepare(f)
  }
}