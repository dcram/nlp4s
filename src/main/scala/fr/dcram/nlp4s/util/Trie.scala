package fr.dcram.nlp4s.util

case class Trie[K,V,Tok](value:Option[V], children:Map[K, Trie[K, V, Tok]], tokPreparator:Tok => K) {
  def get(key:Iterable[K]):Option[V] = key match {
    case k :: tail => children.get(k) match {
      case Some(childTrie) =>
        childTrie.get(tail)
      case None =>
        None
    }
    case Nil =>
      value
  }

  def tokGet(key:Iterable[Tok]):Option[V] = get(key.map(tokPreparator))

}

object Trie {

  def fromEntries[K,V,Tok](entries:Iterable[(Seq[K], V)], tokPreparator:Tok => K):Trie[K,Iterable[V], Tok] = {
    val (empties, nonEmpties) = entries.partition(_._1.isEmpty)
    new Trie(
      value = if(empties.isEmpty) None else Some(empties.map(_._2)),
      children = nonEmpties
        .map{case (k,v) => (k.head, k.tail, v)}
        .groupBy(_._1)
        .mapValues(list => fromEntries(list.map{case (_, seq, value) => (seq, value)}, tokPreparator)),
      tokPreparator = tokPreparator
    )
  }
}
