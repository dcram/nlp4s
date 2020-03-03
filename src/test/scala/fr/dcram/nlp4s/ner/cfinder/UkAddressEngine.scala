package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.Nlp4s
import fr.dcram.nlp4s.ner._
import fr.dcram.nlp4s.tokenizer.Tokenizer

class UkAddressEngine extends NerEngine[NerAddress] {

  override def tokenizer: Tokenizer = Nlp4s.tokenizer("uk")

  private val streetTypes = NerResource.asMap("resource://fr/street-types.map", sep = ',')

  private object StreetNum extends RegexMatcher("""\d+""".r)
  private val StreetType = SetMatcher(streetTypes.keys.toSeq:_*) ~> StringMatcher(".").?
  private val Zip = RegexMatcher("""\b\d{5}\b""".r) | (RegexMatcher("""\b\d{2}\b""".r) ~> RegexMatcher("""\b\d{3}\b""".r))
  private object City extends TxtMatcher(_.charAt(0).isUpper)
  private object Sep extends SetMatcher(",", "-")
  private object StreetNameW extends RegexMatcher("""[\w-']+""".r)

  override def toNameEntity(m: NerMatch): NerAddress = NerAddress(
    num = m.textOpt("num"),
    streetType = m.textOpt("streetType").map(_.replaceAll("\\.", "").trim).flatMap(streetTypes.get),
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
