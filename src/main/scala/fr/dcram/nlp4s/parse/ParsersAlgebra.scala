package fr.dcram.nlp4s.parse

trait ParsersAlgebra[P[+_]] {
  self =>

  implicit def ops[A](p:P[A]):Ops[A] = Ops(p)

  def succeed[A](a: A): P[A]

  // runs two parsers in sequence
  def map2[A,B](p1:P[A], p2: => P[B]): P[(A,B)]

  def map[A,B](p:P[A])(f: A => B): P[B]
  def flatMap[A,B](p:P[A])(f: A => P[B]): P[B]
  def or[A,B>:A](p1:P[A], p2: => P[B]): P[B]

  def opt[A](p:P[A]): P[Option[A]] = p.map(Some.apply) or succeed(None)
  def rep[A](n:Int)(p:P[A]): P[List[A]] = List.fill(n)(p)
    .foldRight(succeed(List.empty[A])){
      case (e, acc) => (e seq acc).map{case (head,tail) => head :: tail}
    }
  def star[A](p:P[A]): P[List[A]] = flatMap(opt(p)){
    case Some(a) => star(p).map(list => a :: list)
    case None => succeed(List.empty)
  }
  def plus[A](p:P[A]): P[List[A]] = (p seq star(p)).map{case (a, list) => a :: list}
  def atMostN[A](n:Int)(p:P[A]): P[List[A]] = p.opt.rep(n).map(_.flatten)
  def atLeastN[A](n:Int)(p:P[A]): P[List[A]] = (p.rep(n) seq star(p)).map{case (l1, l2) => l1 ++ l2}
  def mn[A](m:Int, n:Int)(p:P[A]): P[List[A]] = if(n <= m) p.rep(n) else (p.rep(m) seq p.atMostN(n-m)).map{case (l1, l2) => l1 ++ l2}

  case class Ops[A](p:P[A]) {
    def map[B](f: A => B): P[B] = self.map(p)(f)
    def or[B>:A](p2:P[B]): P[B] = self.or(p,p2)
    def seq[B](p2:P[B]): P[(A,B)] = self.map2(p,p2)
    def ~[B](p2:P[B]): P[(A,B)] = this.seq(p2)
    def rep(n:Int): P[List[A]] = self.rep(n)(p)
    def n(n:Int): P[List[A]] = self.rep(n)(p)
    def *(): P[List[A]] = self.star(p)
    def +(): P[List[A]] = self.plus(p)
    def opt: P[Option[A]] = self.opt(p)
    def atMostN(n:Int): P[List[A]] = self.atMostN(n)(p)
    def atLeastN(n:Int): P[List[A]] = self.atLeastN(n)(p)
    def mn(m:Int, n:Int): P[List[A]] = self.mn(m,n)(p)
  }
  def seq[A1,A2](p1:P[A1], p2: => P[A2]):P[(A1,A2)] = p1 seq p2
  def seq[A1,A2,A3](p1:P[A1], p2: => P[A2], p3: => P[A3]):P[(A1,A2,A3)] = (p1 seq p2 seq p3).map{case ((a1,a2),a3) => (a1, a2, a3)}
  def seq[A1,A2,A3,A4](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4]):P[(A1,A2,A3,A4)] = (p1 seq p2 seq p3 seq p4).map{case (((a1,a2),a3),a4) => (a1, a2, a3, a4)}
  def seq[A1,A2,A3,A4,A5](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4], p5: => P[A5]):P[(A1,A2,A3,A4,A5)] = (p1 seq p2 seq p3 seq p4 seq p5).map{case ((((a1,a2),a3),a4),a5) => (a1, a2, a3, a4, a5)}
  def seq[A1,A2,A3,A4,A5,A6](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4], p5: => P[A5], p6: => P[A6]):P[(A1,A2,A3,A4,A5,A6)] = (p1 seq p2 seq p3 seq p4 seq p5 seq p6).map{case (((((a1,a2),a3),a4),a5),a6) => (a1, a2, a3, a4, a5, a6)}
  def seq[A1,A2,A3,A4,A5,A6,A7](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4], p5: => P[A5], p6: => P[A6], p7: => P[A7]):P[(A1,A2,A3,A4,A5,A6,A7)] = (p1 seq p2 seq p3 seq p4 seq p5 seq p6 seq p7).map{case ((((((a1,a2),a3),a4),a5),a6),a7) => (a1, a2, a3, a4, a5, a6, a7)}
  def seq[A1,A2,A3,A4,A5,A6,A7,A8](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4], p5: => P[A5], p6: => P[A6], p7: => P[A7], p8: => P[A8]):P[(A1,A2,A3,A4,A5,A6,A7,A8)] = (p1 seq p2 seq p3 seq p4 seq p5 seq p6 seq p7 seq p8).map{case (((((((a1,a2),a3),a4),a5),a6),a7),a8) => (a1, a2, a3, a4, a5, a6, a7, a8)}
  def seq[A1,A2,A3,A4,A5,A6,A7,A8,A9](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4], p5: => P[A5], p6: => P[A6], p7: => P[A7], p8: => P[A8], p9: => P[A9]):P[(A1,A2,A3,A4,A5,A6,A7,A8,A9)] = (p1 seq p2 seq p3 seq p4 seq p5 seq p6 seq p7 seq p8 seq p9).map{case ((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9) => (a1, a2, a3, a4, a5, a6, a7, a8, a9)}
  def seq[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](p1:P[A1], p2: => P[A2], p3: => P[A3], p4: => P[A4], p5: => P[A5], p6: => P[A6], p7: => P[A7], p8: => P[A8], p9: => P[A9], p10: => P[A10]):P[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)] = (p1 seq p2 seq p3 seq p4 seq p5 seq p6 seq p7 seq p8 seq p9 seq p10).map{case (((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9),a10) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)}

  def or[A](p1:P[A], p2: => P[A], p3: => P[A]):P[A] = p1 or p2 or p3
  def or[A](p1:P[A], p2: => P[A], p3: => P[A], p4: => P[A]):P[A] = p1 or p2 or p3 or p4
  def or[A](p1:P[A], p2: => P[A], p3: => P[A], p4: => P[A], p5: => P[A]):P[A] = p1 or p2 or p3 or p4 or p5
  def or[A](p1:P[A], p2: => P[A], p3: => P[A], p4: => P[A], p5: => P[A], p6: => P[A]):P[A] = p1 or p2 or p3 or p4 or p5 or p6
  def or[A](p1:P[A], p2: => P[A], p3: => P[A], p4: => P[A], p5: => P[A], p6: => P[A], p7: => P[A]):P[A] = p1 or p2 or p3 or p4 or p5 or p6 or p7
  def or[A](p1:P[A], p2: => P[A], p3: => P[A], p4: => P[A], p5: => P[A], p6: => P[A], p7: => P[A], p8: => P[A]):P[A] = p1 or p2 or p3 or p4 or p5 or p6 or p7 or p8

}
