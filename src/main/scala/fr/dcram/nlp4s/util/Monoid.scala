package fr.dcram.nlp4s.util

trait Monoid[F] {
  def zero:F
  def op(t1: F, t2:F):F
}
