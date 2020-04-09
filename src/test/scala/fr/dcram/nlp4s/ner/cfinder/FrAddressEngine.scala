package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.ner._
import fr.dcram.nlp4s.tokenizer.Tokenizer

class FrAddressEngine extends NerEngine[NerAddress] {

  override def tokenizer: Tokenizer = Tokenizer("""[\(\)\{\}\.,!\?;\:]|(?:[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôâäëïöüÂÄËÏÖÜ]+(?:-[\wßçÇÀâàéèÉÈùÙÊêîÎûÛÔôäëïöüÂÄËÏÖÜ]+){0,3}'?)|[-]""".r)

  private val streetTypes = NerResource
    .asTrie("resource://fr/address-street-types.map", sep = '\t', tokenizer = s => tokenize(s), tokPreparator = _.text.lower)

  private val Dash = SetMatcher("-", "–")
  private val StreetNum = RegexMatcher("""^\d+(?i)(?:bis|b|ter|a|[c-l])?([-/]\d+)?$""".r)
  private val StreetType = TrieMatcher(streetTypes) ~> StringMatcher(".").?
//  private val StreetType = SetMatcher(streetTypes.keys.toSeq:_*).lower ~> StringMatcher(".").?
  private val Zip = RegexMatcher("""\b\d{5}\b""".r) | (RegexMatcher("""\b\d{2}\b""".r) ~> RegexMatcher("""\b\d{3}\b""".r))
  private val CitiWord: TxtMatcher = TxtMatcher(str => str.charAt(0).isUpper && str.lower != "cedex")
  private val City = (SetMatcher("le", "la", "les").lower | SetMatcher("saint", "st").lower).? ~> CitiWord ~> (SetMatcher("sur", "en").lower ~> CitiWord).?
  private object Sep extends SetMatcher(",", "-", "–")
  private val BP = ((SetMatcher("bp").lower | (SetMatcher("b").lower ~> "." ~> SetMatcher("p").lower  )) ~> ".".?) ~> RegexMatcher("""\b\d{2,5}\b""".r).mn(1,2)

  private val StreetWord = RegexMatcher("""[\w']+""".r)

  override def toNameEntity(m: NerMatch): NerAddress = NerAddress(
    num = m.textOpt("num"),
    streetType = m.tokensOpt("streetType").map(_.toSeq).map(tokens => streetTypes.tokGet(tokens).get.head),
    streetName = m.textOpt("streetName"),
    zip = m.text("zip").replaceAll("\\s+", ""),
    city = m.text("city"),
    begin = m.begin,
    end = m.end,
    text = m.text
  )

  rule("one-line")(
    %("num")(StreetNum).?, Sep.?, %("streetType")(StreetType),
    %("streetName")(StreetWord.mn(1,8)),
    Sep.?,
    (BP ~> Sep.?).?,
    %("zip")(Zip), %("city")(City))
}
