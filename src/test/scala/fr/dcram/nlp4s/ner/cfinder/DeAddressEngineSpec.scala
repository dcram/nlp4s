package fr.dcram.nlp4s.ner.cfinder

import org.scalatest.FunSpec

import scala.language.implicitConversions

class DeAddressEngineSpec extends FunSpec {
  describe(classOf[DeAddressEngine].toString) {
    val engine = new DeAddressEngine
    Seq(
      ("Hermannstrasse 13, 20095 Hamburg", Some("13"), Some("Strasse"), Some("Hermann"), "20095", "Hamburg"),
      ("Hermannstrasse 13 B, 20095 Hamburg", Some("13 B"), Some("Strasse"), Some("Hermann"), "20095", "Hamburg"),
      ("Bergmannstr. 11, 30974 Wennigsen", Some("11"), Some("Strasse"), Some("Bergmann"), "30974", "Wennigsen"),
      ("Bergmannstr. 11, 30974 Wennigsen an der Toto", Some("11"), Some("Strasse"), Some("Bergmann"), "30974", "Wennigsen an der Toto"),
      ("Bergmannstr. Nr. 11, 30974 Wennigsen an der Toto", Some("11"), Some("Strasse"), Some("Bergmann"), "30974", "Wennigsen an der Toto"),
      ("Am Forsthaus Gravenbruch 24, 63263 Neu-Isenburg", Some("24"), Some("Bruch"), Some("Am Forsthaus Graven"), "63263", "Neu-Isenburg"),
      ("Im Bangert 1, 64658 Fürth", Some("1"), Some("other"), Some("Im Bang"), "64658", "Fürth"),
      ("Carnaper Straße 48, 42283 Wuppertal", Some("48"), Some("Strasse"), Some("Carnaper"), "42283", "Wuppertal"),
      ("Carl-Legien-Strasse 15, 63073 Offenbach am Main", Some("15"), Some("Strasse"), Some("Carl-Legien-"), "63073", "Offenbach am Main"),
      ("Alter Hof 5, 80331 München", Some("5"), Some("Hof"), Some("Alter"), "80331", "München"),
      ("Gregor-Mendel-Straße 15 B, 53881 Euskirchen", Some("15 B"), Some("Strasse"), Some("Gregor-Mendel-"), "53881", "Euskirchen"),
      ("Schmerzker Ring 15, 14776 Brandenburg an der Havel", Some("15"), Some("Ring"), Some("Schmerzker"), "14776", "Brandenburg an der Havel"),
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
