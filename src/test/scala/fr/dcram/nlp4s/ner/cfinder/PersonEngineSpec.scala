package fr.dcram.nlp4s.ner.cfinder

import org.scalatest.FunSpec

import scala.language.implicitConversions

class PersonEngineSpec extends FunSpec {
  describe(classOf[PersonEngine].toString) {
    val engine = new PersonEngine
    Seq(
      ("Le chapeau de François Dupont est rond.", 1, None, None, Some("François"), "Dupont"),
      ("Le chapeau d'Arnaud des Mares de Glatigny de la Bastille d'Hust est rond.", 1, None, None, Some("Arnaud"), "des Mares de Glatigny"),
      ("Me Giscard d'Estaing a rédigé la constitution européenne.",1,  Some("Me"), Some("Maître"), None, "Giscard d'Estaing"),
      ("Valéry Giscard d'Estaing a rédigé la constitution européenne.", 1, None, None, Some("Valéry"), "Giscard d'Estaing"),
      ("Le chapeau de M. Dupont est rond.", 1, Some("M."), Some("Monsieur"), None, "Dupont"),
      ("Le chapeau de M. Jean-Abdullah Zhang est rond.", 1, Some("M."), Some("Monsieur"), Some("Jean-Abdullah"), "Zhang"),
      ("L'AGE a nommé en qualité de Gérant M. MARITON Bernard.", 1, Some("M."), Some("Monsieur"), Some("Bernard"), "MARITON"),
      ("Le chapeau de M. Aymé Jacquet est rond.", 1, Some("M."), Some("Monsieur"), Some("Aymé"), "Jacquet"),
      ("Bonnet A, Bonnet B, Bonnet Ab", 1, None, None, Some("Bonnet"), "Ab")
    ).foreach {
      case (sentence, n, title, titleLemma, firstname, lastname) =>
        describe(s"Sentence: $sentence") {
          val persons = engine.findAllMatchesIn(sentence)
          assert(persons.size == n)
          val a = persons.head
          it(s"should extract firstname $firstname"){assert(a.firstname == firstname)}
          it(s"should extract lastname $lastname"){assert(a.lastname == lastname)}
          it(s"should extract title $title"){assert(a.title == title.map(t => Title(t, titleLemma.get)))}
        }
    }
  }
}
