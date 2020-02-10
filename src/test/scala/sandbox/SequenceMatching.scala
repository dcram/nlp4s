package sandbox

object SequenceMatching extends App {
  val seq = Seq("A", "b", "c", "D")

  def scan(seq:Seq[String]):Unit = {
    seq match {
      case e +: tail if e.head.isUpper =>
        println(s"*$e*")
        scan(tail)
      case e +: tail =>
        println(e)
        scan(tail)
      case Nil =>
    }
  }

  scan(seq)
}
