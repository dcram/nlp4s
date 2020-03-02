package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.Nlp4s
import org.scalatest.FunSpec

class AddressEngineSpec extends FunSpec {

  describe(classOf[FrAddressEngine].toString) {
    val engine = new FrAddressEngine
    Seq(
      ("10 rue Paul Blanchard , 44000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rue Paul Blanchard 44000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rue Paul Blanchard 44 000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44 000", "Nantes")),
    ).map {
      case ((sentence, Some(address@(num, streetType, streetName, zip, city)))) =>
        it(s"should extract ${address} in $sentence") {
          val seq = Nlp4s.tokenizer("fr").tokenize(sentence)
          val addresses = engine.findAllMatchesIn(seq)
          assert(addresses.size == 1)
          val a = addresses.head
          assert(a.num == num)
          assert(a.streetType == streetType)
          assert(a.streetName == streetName)
          assert(a.zip == zip)
          assert(a.city == city)
        }
    }

    def bm(n:Int, f:()=>Unit):Long = {
      implicit def intWithTimes(n: Int) = new {
        def times(f: => Unit) = 1 to n foreach {_ => f}
      }

      val s = System.currentTimeMillis()
      n.times(f())
      val e = System.currentTimeMillis()
      e-s
    }
    it("bm1") {
      val seq = Nlp4s.tokenizer("fr").tokenize("10 rue Paul Blanchard , 44000 Nantes")
      for(n <- Seq(1,10,100,1000,10000,100000)) {
        val duration = bm(n, () => engine.findAllMatchesIn(seq))
        println(f"$n%8d times -> $duration%d ms")
      }
    }
  }
}
