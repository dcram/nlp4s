package fr.dcram.nlp4s.automata

import fr.dcram.nlp4s.util.Trie
import org.scalatest.FunSpec

class TrieSpec extends FunSpec {
  describe(Trie.toString) {
    describe("#get") {
      val t = Trie.fromEntries[String, Int, String](Seq(
        (List("a", "b"), 10),
        (List("a", "c"), 11),
        (List("a", "b", "d"), 12),
        (List("a", "b"), 13),
      ), identity)

      Seq(
        (List("a"), None),
        (List("a", "b"), Some(Iterable(10,13))),
        (List("a", "c"), Some(Iterable(11))),
        (List("a", "b", "c"), None),
        (List("a", "b", "d"), Some(Iterable(12))),
      ).foreach {
        case (k,v) =>
          it(s"should return $v for k=$k") {
            assert(t.get(k) == v)
          }
      }
    }

  }
}