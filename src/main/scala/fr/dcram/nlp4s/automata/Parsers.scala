package fr.dcram.nlp4s.automata

trait Parsers[Tok, P[Tok, +_]] {
  pp =>

  implicit def ops[A](p:P[Tok,A]):ParserOps[A] = ParserOps(p)

  def succeed[A](a: A): P[Tok, A]
  def tok(f: Tok => Boolean): P[Tok, Tok] = fromOpt(tok => if(f(tok)) Some(tok) else None)
  def fromOpt[A](f: Tok => Option[A]): P[Tok, A]
  def parse[A](p:P[Tok, A])(seq:Seq[Tok]):Option[A]
  def scan[A](p:P[Tok, A])(seq:Seq[Tok]):Stream[A]
  def ~[A,B](p1:P[Tok, A], p2: => P[Tok, B]): P[Tok, (A,B)]
  def map[A,B](p:P[Tok, A])(f: A => B): P[Tok, B]
  def flatMap[A,B](p:P[Tok, A])(f: A => P[Tok, B]): P[Tok, B]
  def or[A,B>:A](p1:P[Tok, A], p2: => P[Tok, B]): P[Tok, B]
  def opt[A](p:P[Tok, A]): P[Tok, Option[A]] = p.map(Some.apply) or succeed(None)
  def rep[A](n:Int)(p:P[Tok, A]): P[Tok, List[A]] = List.fill(n)(p)
    .foldRight(succeed(List.empty[A])){
      case (e, acc) => (e ~ acc).map{case (head,tail) => head :: tail}
    }
  def star[A](p:P[Tok, A]): P[Tok, List[A]] = flatMap(opt(p)){
    case Some(a) => star(p).map(list => a :: list)
    case None => succeed(List.empty)
  }
  def plus[A](p:P[Tok, A]): P[Tok, List[A]] = (p ~ star(p)).map{case (a, list) => a :: list}
  def atMostN[A](n:Int)(p:P[Tok, A]): P[Tok, List[A]] = p.opt.rep(n).map(_.flatten)
  def atLeastN[A](n:Int)(p:P[Tok, A]): P[Tok, List[A]] = (p.rep(n) ~ p.*).map{case (l1, l2) => l1 ++ l2}
  def mn[A](m:Int, n:Int)(p:P[Tok, A]): P[Tok, List[A]] = (p.rep(m) ~ p.atMostN(n)).map{case (l1, l2) => l1 ++ l2}


  case class ParserOps[A](p:P[Tok,A]) {
    def parse(seq:Seq[Tok]):Option[A] = pp.parse(p)(seq)
    def scan(seq:Seq[Tok]):Stream[A] = pp.scan(p)(seq)
    def map[B](f: A => B): P[Tok, B] = pp.map(p)(f)
    def or[B>:A](p2:P[Tok, B]): P[Tok, B] = pp.or(p,p2)
    def ~[B](p2:P[Tok, B]): P[Tok, (A,B)] = pp.~(p,p2)
    def rep(n:Int): P[Tok, List[A]] = pp.rep(n)(p)
    def n(n:Int): P[Tok, List[A]] = pp.rep(n)(p)
    def opt: P[Tok, Option[A]] = pp.opt(p)
    def ?(): P[Tok, Option[A]] = pp.opt(p)
    def |(): P[Tok, Option[A]] = pp.opt(p)
    def *(): P[Tok, List[A]] = pp.star(p)
    def +(): P[Tok, List[A]] = pp.plus(p)
    def atMostN(n:Int): P[Tok, List[A]] = pp.atMostN(n)(p)
    def atLeastN(n:Int): P[Tok, List[A]] = pp.atLeastN(n)(p)
    def mn(m:Int, n:Int): P[Tok, List[A]] = pp.mn(m,n)(p)
  }
}
