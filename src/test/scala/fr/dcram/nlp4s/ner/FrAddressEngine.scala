package fr.dcram.nlp4s.ner

class FrAddressEngine extends NerEngine[Address] {

  object StreetNum extends RegexMatcher("""\d+""".r)
  object StreetType extends SetMatcher("rue", "place", "avenue")
  val Zip = RegexMatcher("""\b\d{5}\b""".r) | (RegexMatcher("""\b\d{2}\b""".r) ~> RegexMatcher("""\b\d{3}\b""".r))
  object City extends TxtMatcher(_.charAt(0).isUpper)
  object Sep extends SetMatcher(",", "-")
  object StreetNameW extends RegexMatcher("""[\w-']+""".r)

  override def toNameEntity(m: NerMatch): Address = Address(
    num = m.textOpt("num"),
    streetType = m.textOpt("streetType"),
    streetName = m.textOpt("streetName"),
    zip = m.text("zip"),
    city = m.text("city"),
    begin = m.begin,
    end = m.end,
    text = m.text,
  )

  rule("one-line")(
    %("num")(StreetNum), %("streetType")(StreetType), %("streetName")(StreetNameW.mn(1,8)),
    Sep.?,
    %("zip")(Zip), %("city")(City))
}
