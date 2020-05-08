package fr.dcram.nlp4s.util

case class Trie[K,+V](value:Option[V], children:Map[K, Trie[K, V]]) {
  def getValue(key:Iterable[K]):Option[V] = key match {
    case k :: tail => children.get(k) match {
      case Some(childTrie) =>
        childTrie.getValue(tail)
      case None =>
        None
    }
    case Nil =>
      value
  }

  def getChild(k:K):Option[Trie[K,V]] = children.get(k)

  def mapKey[K2](f:K => K2):Trie[K2,V] = Trie(
    value,
    children.map{case (k,v) => (f(k),v.mapKey(f))}
  )

}

object Trie {

  def fromEntries[K,V](entries:Iterable[(Seq[K], V)]):Trie[K,V] = {
    val (values, nonEmpties) = entries.partition(_._1.isEmpty)
    new Trie(
      value = values.map(_._2).headOption,
      children = nonEmpties
        .map{case (k,v) => (k.head, k.tail, v)}
        .groupBy(_._1)
        .mapValues(list => fromEntries(list.map{case (_, seq, value) => (seq, value)}))
    )
  }
}
