package fr.dcram.nlp4s.util


trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa:F[A])(f: A => F[B]):F[B]
  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(f => map(fa)(f))
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
}
