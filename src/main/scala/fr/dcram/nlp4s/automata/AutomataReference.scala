package fr.dcram.nlp4s.automata

object AutomataReferenceTypes {
  type Parser[Tok, +A] = Seq[Tok] => Stream[Result[Tok, A]]
  case class Result[+Tok, +A](seq:Seq[Tok], m:A)
}

import fr.dcram.nlp4s.automata.AutomataReferenceTypes._

import scala.annotation.tailrec
trait AutomataReference[Tok] extends Parsers[Tok, Parser] {

  override def succeed[A](a: A): Parser[Tok, A] = seq => Stream(Result(seq, a))

  override def tokA[A](f: Tok => Option[A]): Parser[Tok, A] = seq => seq match {
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
}
