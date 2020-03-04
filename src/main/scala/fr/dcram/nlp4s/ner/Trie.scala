package fr.dcram.nlp4s.ner
case class Trie[Tok,V](value:Option[V], children:Map[Tok, Trie[Tok, V]]) {
  def get(key:List[Tok]):Option[V] = key match {
    case k :: tail => children.get(k) match {
      case Some(childTrie) => childTrie.get(tail)
      case None => None
    }
    case Nil => None
  }

//  def asAutomaton:Automaton[Tok] = {
//    Automaton(State[Tok](idGen.incrementAndGet(), Seq.empty, accepting = children.isEmpty))
//  }
}

object Trie {

  def apply[K,V](entries:Iterable[(Seq[K], V)]):Trie[K,Iterable[V]] = {
    val (empties, nonEmpties) = entries.partition(_._1.isEmpty)
    new Trie(
      value = if(empties.isEmpty) None else Some(empties.map(_._2)),
      children = nonEmpties
        .map{case (k,v) => (k.head, k.tail, v)}
        .groupBy(_._1)
        .mapValues(list => apply(list.map{case (_, seq, value) => (seq, value)}))
    )
  }
}
