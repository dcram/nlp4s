package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.Nlp4s
import fr.dcram.nlp4s.ner.{StringMatcher, _}
import fr.dcram.nlp4s.tokenizer.Tokenizer

class PersonEngine extends NerEngine[NerPerson] {

  override def tokenizer: Tokenizer = Nlp4s.tokenizer("fr")
  private val titles = NerResource.asMap("resource://fr/person-titles.map", sep = '\t')
  private val firstnames = NerResource.asSet("resource://fr/person-firstnames.set")

  private val TitleM = SetMatcher(titles.keys.toSeq:_*) ~> StringMatcher(".").?

  private object FirstnameWord extends TxtMatcher(str => str.charAt(0).isUpper && str.length > 1 && firstnames.contains(str.lower))
  val Firstname = FirstnameWord ~> ("-" ~> FirstnameWord).?
  private object LastnameWord extends TxtMatcher(str => str.charAt(0).isUpper && str.length > 1)
  private object D extends SetMatcher("de", "d'", "des", "du")
  private object Le extends SetMatcher("la", "le")
  private val Lastname = (D ~> Le.?).? ~> LastnameWord ~> (D ~> Le.? ~> LastnameWord).?
  override def toNameEntity(m: NerMatch): NerPerson = NerPerson(
    title = m.groups("title").headOption.map(g => Title(g.text, titles.get(g.text).get)),
    firstname = m.textOpt("firstname"),
    lastname = m.text("lastname"),
    begin = m.begin,
    end = m.end,
    text = m.text,
  )

  rule("title-lastname-firstname?")(
    %("title")(TitleM),
    (%("firstname")(Firstname) ~> %("lastname")(Lastname) | (%("lastname")(Lastname) ~> %("firstname")(FirstnameWord).?))
  )
  rule("firstname-lastname")(%("firstname")(Firstname), %("lastname")(Lastname))
}
