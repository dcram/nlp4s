package fr.dcram.nlp4s.ner.cfinder

import org.scalatest.FunSpec

import scala.language.implicitConversions

class UkAddressEngineSpec extends FunSpec {
  describe(classOf[UkAddressEngine].toString) {
    val engine = new UkAddressEngine
    Seq(
      ("64 New Cavendish Street, London W1G 8TB", Some("64"), Some("street"), Some("New Cavendish"), "W1G 8TB", "London"),
      ("Unit R4, HAYDN ROAD, Cheshire, West Yorkshire, London NW4 4QE", Some("R4"), Some("road"),  Some("HAYDN"), "NW4 4QE", "London"),
      ("292A HAYDN ROAD London NW4 4QE", Some("292A"), Some("road"), Some("HAYDN"), "NW4 4QE", "London"),
      ("292 HAYDN ROAD London NW4 4QE", Some("292"), Some("road"), Some("HAYDN"), "NW4 4QE", "London"),
      ("291-291 HAYDN ROAD London NW4 4QE", Some("291-291"), Some("road"), Some("HAYDN"), "NW4 4QE", "London"),
      ("Unit R4, HAYDN ROAD London NW4 4QE", Some("R4"), Some("road"), Some("HAYDN"), "NW4 4QE", "London"),
      ("292 HAYDNROAD London NW4 4QE", Some("292"), Some("road"), Some("HAYDN"), "NW4 4QE", "London"),
      ("Unit R4 HAYDN ROAD London NW4 4QE", Some("R4"), Some("road"), Some("HAYDN"), "NW4 4QE", "London"),
      ("Leigh House 28-32 St Paul's Street Leeds LS1 2JT", Some("28-32"), Some("street"), Some("St Paul's"), "LS1 2JT", "Leeds")
    ).foreach {
      case (sentence, number, streetType, streetName, zipCode, city) =>
        val n = 1
        describe(sentence) {
          val addresses = engine.findAllMatchesIn(sentence)
          it(s"should extract $n address(es)") {
            assert(addresses.size == 1)
          }
          addresses.headOption.foreach {
            a =>
              it(s"should extract num $number") {
                assert(a.num == number)
              }
              it(s"should extract streetType $streetType") {
                assert(a.streetType == streetType)
              }
              it(s"should extract streetName $streetName") {
                assert(a.streetName == streetName)
              }
              it(s"should extract zip $zipCode") {
                assert(a.zip == zipCode)
              }
              it(s"should extract city $city") {
                assert(a.city == city)
              }
          }
        }
    }
  }
}
