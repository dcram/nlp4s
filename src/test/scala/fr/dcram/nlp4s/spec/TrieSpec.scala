package fr.dcram.nlp4s.spec

import fr.dcram.nlp4s.util.Trie
import org.scalatest.FunSpec

class TrieSpec extends FunSpec {
  describe(Trie.toString) {
    describe("#get") {
      val t = Trie.fromEntries[String, Int](Seq(
        (List("a", "b"), 10),
        (List("a", "c"), 11),
        (List("a", "b", "d"), 12),
        (List("a", "b"), 13),
      ))

      Seq(
        (List("a"), None),
        (List("a", "b"), Some(10)),
        (List("a", "c"), Some(11)),
        (List("a", "b", "c"), None),
        (List("a", "b", "d"), Some(12)),
      ).foreach {
        case (k,v) =>
          it(s"should return $v for k=$k") {
            assert(t.getValue(k) == v)
          }
      }
    }

  }
}