package fr.dcram.nlp4s.automata

import fr.dcram.nlp4s.util.IdGen


case class Trie[K,V,Tok](value:Option[V], children:Map[K, Trie[K, V, Tok]], tokPreparator:Tok => K) {

  case class TrieTransition(children:Map[K, Trie[K, V, Tok]])(implicit idGen:IdGen) extends ExtrinsicTransition[Tok] {
    override def _match(s: Seq[Token[Tok]]): Option[(State[Tok], Seq[Token[Tok]], Seq[Token[Tok]])] = {
      s match {
        case (ut@UserToken(tok)) +: rest =>
          children.get(tokPreparator(tok)) match {
            case Some(childTrie) =>
              Some((childTrie.asState, Seq(ut), rest))
            case None => None
          }
        case _ =>
          None
      }
    }
  }

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

  def transitions()(implicit idGen:IdGen):Seq[Transition[Tok]] = Seq(TrieTransition(children))

  private def asState(implicit idGen:IdGen):State[Tok] = State(
      idGen.next,
      transitions = transitions,
      accepting = value.isDefined
    )

  def asAutomaton:Automaton[Tok] = {
    implicit val idGen = IdGen()
    Automaton(asState)
  }
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
