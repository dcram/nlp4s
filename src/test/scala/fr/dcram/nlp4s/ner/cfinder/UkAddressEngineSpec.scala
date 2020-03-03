package fr.dcram.nlp4s.ner.cfinder

import org.scalatest.FunSpec

import scala.language.implicitConversions

class UkAddressEngineSpec extends FunSpec {
  describe(classOf[UkAddressEngine].toString) {
    val engine = new UkAddressEngine
    Seq(
      ("Unit R4, HAYDN ROAD, Cheshire, West Yorkshire, London NW4 4QE", Some("R4"), Some("road"), None, Some("HAYDN"), Some("HAYDN"), Some("NW4 4QE"), Some("London")), 
      ("292A HAYDN ROAD London NW4 4QE", Some("292A"), Some("road"), None, Some("HAYDN"), Some("HAYDN"), Some("NW4 4QE"), Some("London")), 
      ("64 New Cavendish Street, London W1G 8TB", Some("64"), Some("street"), None, Some("New Cavendish"), Some("New Cavendish"), Some("W1G 8TB"), Some("London")), 
      ("292 HAYDN ROAD London NW4 4QE", Some("292"), Some("road"), None, Some("HAYDN"), Some("HAYDN"), Some("NW4 4QE"), Some("London")), 
      ("291-291 HAYDN ROAD London NW4 4QE", Some("291-291"), Some("road"), None, Some("HAYDN"), Some("HAYDN"), Some("NW4 4QE"), Some("London")), 
      ("Unit R4, HAYDN ROAD London NW4 4QE", Some("R4"), Some("road"), None, Some("HAYDN"), Some("HAYDN"), Some("NW4 4QE"), Some("London")), 
      ("292 HAYDNROAD London NW4 4QE", Some("292"), Some("road"), None, Some("HAYDNROAD"), Some("HAYDNROAD"), Some("NW4 4QE"), Some("London")), 
      ("Unit R4 HAYDN ROAD London NW4 4QE", Some("R4"), Some("road"), None, Some("HAYDN"), Some("HAYDN"), Some("NW4 4QE"), Some("London")), 
      ("Leigh House 28-32 St Paul's Street Leeds LS1 2JT", Some("28-32"), Some("street"), None, Some("St Paul's"), Some("St Paul's"), Some("LS1 2JT"), Some("Leeds")), 
    ).foreach {
      case (sentence, number, streetType, street, streetName, streetAForm, zipCode, city) =>
        it(s"should extract address from in $sentence") {
          val addresses = engine.findAllMatchesIn(sentence)
          assert(addresses.size == 1)
          val a = addresses.head
          assert(a.num == number)
          assert(a.streetType == streetType)
          assert(a.streetName == streetName)
          assert(a.zip == zipCode)
          assert(a.city == city)
        }
    }

  }
}
