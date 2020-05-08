package fr.dcram.nlp4s.ner

trait Resources[F[+_]] {

  def prepare[A,B](resource:F[A])(f:A => B):F[B]
}

