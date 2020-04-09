package fr.dcram.nlp4s.ner.cfinder

import org.scalatest.FunSpec

import scala.language.implicitConversions

class FrAddressEngineSpec extends FunSpec {
  describe(classOf[FrAddressEngine].toString) {
    val engine = new FrAddressEngine
    Seq(
      ("Quartier Valmy - Place Ronde 92281 Paris La Défense Cedex", 1,None, Some("place"), Some("Ronde"), "92281", "Paris"),
      ("1 rue Toto 44000 Nantes Nantes", 1, Some("1"), Some("rue"), Some("Toto"), "44000", "Nantes"),
      ("1 rue Toto 44000 Le Pallet Tata", 1, Some("1"), Some("rue"), Some("Toto"), "44000", "Le Pallet"),
      ("1 rue Toto 44000 Saint Sébastien Toto", 1, Some("1"), Some("rue"), Some("Toto"), "44000", "Saint Sébastien"),
      ("3 rond point de Paris, 44000 Nantes", 1, Some("3"), Some("autre"), Some("de Paris"), "44000", "Nantes"),
      ("5 rue du 11 novembre 1918, 44000 NANTES", 1, Some("5"), Some("rue"), Some("du 11 novembre 1918"), "44000", "NANTES"),
      ("7 rue Étienne Larchey, 44 000 Nantes", 1, Some("7"), Some("rue"),  Some("Étienne Larchey"), "44000", "Nantes"),
      ("Mouvement des Dirigeants YOYOGEI S.A.R.L. au capital de 7.622 € Siège social : 128, rue des Champs Lasniers 91940 LES ULIS 729 803 577 RCS Evry L'AGE du 30/12/2015 a nommé en qualité de Gérant M. MARITON Bernard, demeurant 138, place Saint-Jean, 27130 Verneuil-sur Avre, en remplacement de M. MARITON Maurice, à compter du 01/01/2016. Modification au RCS d'Evry. I.S.O. R4111082", 2, Some("128"), Some("rue"), Some("des Champs Lasniers"), "91940", "LES ULIS"),
      ("7ter rue Étienne Larchey, 44 000 Nantes", 1, Some("7ter"), Some("rue"), Some("Étienne Larchey"), "44000", "Nantes"),
      ("38 rue de Liège\n75008 Paris", 1, Some("38"), Some("rue"), Some("de Liège"), "75008", "Paris"),
      ("4 RUE DU GENERAL GALLIENI 97200 FORT DE FRANCE", 1, Some("4"), Some("rue"), Some("DU GENERAL GALLIENI"), "97200", "FORT"),
      ("Château Machin rue Étienne Larchey, 44 000 Nantes", 1,None, Some("rue"), Some("Étienne Larchey"), "44000", "Nantes"),
      ("7-9 rue Étienne Larchey, 44 000 Nantes", 1, Some("7-9"), Some("rue"), Some("Étienne Larchey"), "44000", "Nantes"),
      ("38 rue de Liège - 75008 Paris", 1, Some("38"), Some("rue"), Some("de Liège"), "75008", "Paris"),
      ("4 ALLEE JEAN BART 44000 NANTES", 1, Some("4"), Some("allee"), Some("JEAN BART"), "44000", "NANTES"),
      ("1 RUE DES 3 ORMEAUX 44000 NANTES", 1, Some("1"), Some("rue"), Some("DES 3 ORMEAUX"), "44000", "NANTES"),
      ("37 rue Jules Guesde – 59463 Saint-Rémy sur Loire", 1, Some("37"), Some("rue"), Some("Jules Guesde"), "59463", "Saint-Rémy sur Loire"),
      ("24, Faubourg Saint-Honoré   75008 Paris", 1, Some("24"), Some("autre"), Some("Saint-Honoré"), "75008", "Paris"),
      ("Fondation d'entreprise Hermès   24, Faubourg Saint-Honoré   75008 Paris       12, Rue Boissy d'Anglas   75008 Paris   Maison de la Culture du Japon   101Bis, Quai Branly   75015 Paris", 3, Some("24"), Some("autre"), Some("Saint-Honoré"), "75008", "Paris"),
      ("5687976347853789653796359783983763908 rue Jean Monnet, 44000 Nantes", 1, Some("5687976347853789653796359783983763908"), Some("rue"), Some("Jean Monnet"), "44000", "Nantes"),
      ("4 Rue SANTEUIL 44000 NANTES", 1, Some("4"), Some("rue"), Some("SANTEUIL"), "44000", "NANTES"),
      ("4 r SANTEUIL 44000 NANTES", 1, Some("4"), Some("rue"), Some("SANTEUIL"), "44000", "NANTES"),
      ("4 r. SANTEUIL 44000 NANTES", 1, Some("4"), Some("rue"), Some("SANTEUIL"), "44000", "NANTES"),
      ("4 R SANTEUIL 44000 NANTES", 1, Some("4"), Some("rue"), Some("SANTEUIL"), "44000", "NANTES"),
      ("1 PL ALEX RAYMOND, BP 330, 31770 Colomiers", 1, Some("1"), Some("place"), Some("ALEX RAYMOND"), "31770", "Colomiers"),
      ("1 PL. ALEX RAYMOND, BP 330, 31770 Colomiers", 1, Some("1"), Some("place"), Some("ALEX RAYMOND"), "31770", "Colomiers"),
      ("5a rue Tata 44000 NANTES", 1, Some("5a"), Some("rue"), Some("Tata"), "44000", "NANTES"),
      ("Le Furet du Nord – Service Internet, 37 rue Jules Guesde – B.P. 80 359 – 59463 Lomme Cedex", 1, Some("37"), Some("rue"), Some("Jules Guesde"), "59463", "Lomme")
    ).foreach {
      case (sentence, n, number, streetType, streetName, zip, city) =>
        describe(s"sentence $sentence") {
          val addresses = engine.findAllMatchesIn(sentence)
          it(s"there should be $n address detected") {assert(addresses.size == n)}
          addresses.headOption.map{a => {
            it(s"should find num=$number"){assert(a.num == number)}
            it(s"should find streetType=$streetType"){assert(a.streetType == streetType)}
            it(s"should find streetName=$streetName"){assert(a.streetName == streetName)}
            it(s"should find zip=$zip"){assert(a.zip == zip)}
            it(s"should find city=$city"){assert(a.city == city)}
          }}
        }
    }

  }
}
