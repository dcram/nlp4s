package fr.dcram.nlp4s.ner
import fr.dcram.nlp4s.automata.RegexMatch
import fr.dcram.nlp4s.model.Token

class FrAddressEngine extends NerEngine[Address] {

  object StreetNum extends RegexMatcher("""\d+""".r)
  object StreetType extends SetMatcher("rue", "place", "avenue")
  object Zip extends RegexMatcher("""\d{5}""".r)
  object City extends TxtMatcher(_.charAt(0).isUpper)
  object Sep extends SetMatcher(",", "-")
  object StreetNameW extends RegexMatcher("""[\w-']+""".r)

  override def toNameEntity(m: RegexMatch[Token]): Address = Address(
    num = m.groups.get("num").flatMap(_.headOption).map(_.tokens.map(_.text).mkString(" ")),
    streetType = m.groups.get("streetType").flatMap(_.headOption).map(_.tokens.map(_.text).mkString(" ")),
    streetName = m.groups.get("streetName").flatMap(_.headOption).map(_.tokens.map(_.text).mkString(" ")),
    zip = m.groups("zip").head.tokens.map(_.text).mkString(" "),
    city = m.groups("city").head.tokens.map(_.text).mkString(" "),
    begin = m.tokens.map(_.begin).min,
    end = m.tokens.map(_.end).max,
    text = m.tokens.map(_.text).mkString,
  )

  rule("one-line")(
    %("num")(StreetNum),
    %("streetType")(StreetType),
    %("streetName")(StreetNameW.mn(1,8)),
    Sep?,
    %("zip")(Zip),
    %("city")(City))
}
