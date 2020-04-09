package fr.dcram.nlp4s.util

import java.util.concurrent.atomic.AtomicLong

case class IdGen() {
  private[this] val id = new AtomicLong(0)
  def next:Long = id.incrementAndGet()
}
