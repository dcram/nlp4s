package fr.dcram.nlp4s.util

trait Applicative[F[_]] extends Functor[F]{
  def pure[A](x: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  def map2[A, B, C](fa: F[A], fb: => F[B])(f: (A,B) => C): F[C]
  def sequence[A](list: List[F[A]]):F[List[A]] = list.foldRight(pure(List.empty[A])){case (e, acc) => map2(e, acc)(_ :: _)}
  def product[A, B](fa: F[A], fb: => F[B]): F[(A,B)] = map2(fa, fb)(Tuple2.apply)
  def option[A](opt: Option[F[A]]):F[Option[A]] = opt match {
    case Some(fa) => map(fa)(Some.apply)
    case None => pure(None)
  }

}
