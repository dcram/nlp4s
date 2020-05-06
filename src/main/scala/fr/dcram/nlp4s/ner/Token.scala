package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.util.Applicative

case class Token[+A](begin:Int,end:Int,obj:A) {
  private[this] val app = Token.MergeApplicative
  def map[B](f:A => B):Token[B] = app.map(this)(f)
}

object Token {
  object MergeApplicative extends Applicative[Token] {
    override def map2[A, B, C](fa: Token[A], fb: => Token[B])(f: (A,B) => C): Token[C] = Token(
      if(fa.begin <= fb.begin) fa.begin else fb.begin,
      if(fa.end >= fb.end) fa.end else fb.end,
      f(fa.obj, fb.obj)
    )
    override def map[A, B](fa: Token[A])(f: A => B): Token[B] = fa.copy(obj = f(fa.obj))
    override def pure[A](x: A): Token[A] = Token(Int.MaxValue,Int.MinValue,x)
    override def ap[A, B](ff: Token[A => B])(fa: Token[A]): Token[B] = Token(
      if(fa.begin <= ff.begin) fa.begin else ff.begin,
      if(fa.end >= ff.end) fa.end else ff.end,
      ff.obj(fa.obj)
    )

    // arities
    def map3[A, B, C, D](fa: Token[A], fb: => Token[B], fc: => Token[C])(f: (A,B,C) => D): Token[D] = Token(
      Iterator(fa, fb, fc).minBy(_.begin).begin,
      Iterator(fa, fb, fc).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj)
    )
    def map4[A, B, C, D, E](fa: Token[A], fb: => Token[B], fc: => Token[C], fd: => Token[D])(f: (A,B,C,D) => E): Token[E] = Token(
      Iterator(fa, fb, fc, fd).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj)
    )
    def map5[A, B, C, D, E, F](fa: Token[A], fb: => Token[B], fc: => Token[C], fd: => Token[D], fe: => Token[E])(f: (A,B,C,D,E) => F): Token[F] = Token(
      Iterator(fa, fb, fc, fd, fe).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd, fe).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj, fe.obj)
    )
    def map6[A, B, C, D, E, F, G](fa: Token[A], fb: => Token[B], fc: => Token[C], fd: => Token[D], fe: => Token[E], ff: => Token[F])(f: (A,B,C,D,E,F) => G): Token[G] = Token(
      Iterator(fa, fb, fc, fd, fe, ff).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd, fe, ff).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj, fe.obj, ff.obj)
    )

    def map7[A, B, C, D, E, F, G, H](
                                                fa: Token[A],
                                                fb: => Token[B],
                                                fc: => Token[C],
                                                fd: => Token[D],
                                                fe: => Token[E],
                                                ff: => Token[F],
                                                fg: => Token[G],
                                              )(f: (A,B,C,D,E,F,G) => H): Token[H] = Token(
      Iterator(fa, fb, fc, fd, fe, ff, fg).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd, fe, ff, fg).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj, fe.obj, ff.obj, fg.obj)
    )

    def map8[A, B, C, D, E, F, G, H, I](
                                                fa: Token[A],
                                                fb: => Token[B],
                                                fc: => Token[C],
                                                fd: => Token[D],
                                                fe: => Token[E],
                                                ff: => Token[F],
                                                fg: => Token[G],
                                                fh: => Token[H],
                                              )(f: (A,B,C,D,E,F,G,H) => I): Token[I] = Token(
      Iterator(fa, fb, fc, fd, fe, ff, fg, fh).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd, fe, ff, fg, fh).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj, fe.obj, ff.obj, fg.obj, fh.obj)
    )

    def map9[A, B, C, D, E, F, G, H, I, J](
                                                fa: Token[A],
                                                fb: => Token[B],
                                                fc: => Token[C],
                                                fd: => Token[D],
                                                fe: => Token[E],
                                                ff: => Token[F],
                                                fg: => Token[G],
                                                fh: => Token[H],
                                                fi: => Token[I],
                                              )(f: (A,B,C,D,E,F,G,H,I) => J): Token[J] = Token(
      Iterator(fa, fb, fc, fd, fe, ff, fg, fh, fi).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd, fe, ff, fg, fh, fi).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj, fe.obj, ff.obj, fg.obj, fh.obj, fi.obj)
    )

    def map10[A, B, C, D, E, F, G, H, I, J, K](
                                                fa: Token[A],
                                                fb: => Token[B],
                                                fc: => Token[C],
                                                fd: => Token[D],
                                                fe: => Token[E],
                                                ff: => Token[F],
                                                fg: => Token[G],
                                                fh: => Token[H],
                                                fi: => Token[I],
                                                fj: => Token[J],
                                              )(f: (A,B,C,D,E,F,G,H,I,J) => K): Token[K] = Token(
      Iterator(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj).minBy(_.begin).begin,
      Iterator(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj).maxBy(_.end).end,
      f(fa.obj, fb.obj, fc.obj, fd.obj, fe.obj, ff.obj, fg.obj, fh.obj, fi.obj, fj.obj)
    )

  }

}

