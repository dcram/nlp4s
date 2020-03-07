package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.model.Token
import fr.dcram.nlp4s.ner._
import fr.dcram.nlp4s.tokenizer.Tokenizer

class UkAddressEngine extends NerEngine[NerAddress] {

  override def tokenizer: Tokenizer = Tokenizer("""[\(\)\{\}\.,!\?;\:]|(?:['\w]+(?:-\w+)?)|[-]""".r)

  private val Unit = SetMatcher("unit", "wing", "box", "bloc", "flat", "building").lower
  private val UnitNum = RegexMatcher("""^(\d+([-]\d+)?[A-Z]?)|([A-Z]\d*)$""".r)
  private val roadTypes = NerResource.asMap("resource://uk/address-street-types.map", sep = '\t').map{case (k,v) => (k.lower, v)}

  private object StreetNum extends RegexMatcher("""^\d+([-]\d+)?[A-Z]?$""".r)

  private val RoadType = SetMatcher(roadTypes.keys.toSeq:_*).lower
  private object MorphoRoadType extends NerTokenMatcher {
    def split(str:String):Option[(String,String)] = {
      if(str.length <= 6)
        None
      else
        (3 to str.length-3).toStream
          .map(str.splitAt)
          .filter{case (_,part2) => roadTypes.contains(part2.lower)}
          .headOption
    }
    override def matches(tok: Token): Boolean = split(tok.text).nonEmpty
  }
  private object Capped extends TxtMatcher(_.charAt(0).isUpper)
  private val City = Capped ~> (SetMatcher("on", "under", "upon", "and").lower ~> Capped).?
  private object Sep extends SetMatcher(",", ".", "-", "―", "—", "–", "‒", "-",  "/")

  private object UkDistrict extends NerTokenMatcher {
    private val Areas = Set("AB",  "AL",  "B",  "BA",  "BB",  "BD",  "BF",  "BH",  "BL",  "BN",  "BR",  "BS",  "BT",  "CA",  "CB",  "CF",  "CH",  "CM",  "CO",  "CR",  "CT",  "CV",  "CW",  "DA",  "DD",  "DE",  "DG",  "DH",  "DL",  "DN",  "DT",  "DY",  "E",  "EC",  "EH",  "EN",  "EX",  "FK",  "FY",  "G",  "GIR",  "GL",  "GU",  "HA",  "HD",  "HG",  "HP",  "HR",  "HS",  "HU",  "HX",  "IG",  "IP",  "IV",  "KA",  "KT",  "KW",  "KY",  "L",  "LA",  "LD",  "LE",  "LL",  "LN",  "LS",  "LU",  "M",  "ME",  "MK",  "ML",  "N",  "NE",  "NG",  "NN",  "NP",  "NPT",  "NR",  "NW",  "OL",  "OX",  "PA",  "PE",  "PH",  "PL",  "PO",  "PR",  "RG",  "RH",  "RM",  "S",  "SA",  "SE",  "SG",  "SK",  "SL",  "SM",  "SN",  "SO",  "SP",  "SR",  "SS",  "ST",  "SW",  "SY",  "TA",  "TD",  "TF",  "TN",  "TQ",  "TR",  "TS",  "TW",  "UB",  "W",  "WA",  "WC",  "WD",  "WF",  "WN",  "WR",  "WS",  "WV",  "YO",  "ZE")
    private val District = """^(([A-Z]{1,3})\d{1,2}[A-Z]?)$""".r("district", "area")
    override def matches(tok: Token): Boolean = {
      District.findFirstMatchIn(tok.text)
        .filter(m => Areas.contains(m.group("area")))
        .isDefined
    }
  }
  private val UkPostcode = UkDistrict ~> RegexMatcher("""^[0-9A-Z][A-Z]{2}$""".r)

  override def toNameEntity(m: NerMatch): NerAddress = {
    val morphoRoadOpt:Option[(String,String)] = m.textOpt("morphoRoadType").flatMap(MorphoRoadType.split)
    val (roadOpt, streetNameOpt) = morphoRoadOpt match {
      case Some((sname,stype)) => (roadTypes.get(stype.lower), Some(sname))
      case _ => (
        m.textOpt("roadType").map(_.replaceAll("\\.", "").trim.lower).flatMap(roadTypes.get),
        m.textOpt("streetName")
      )
    }
    NerAddress(
      num = m.textOpt("num"),
      streetType = roadOpt,
      streetName = streetNameOpt,
      zip = m.text("zip"),
      city = m.text("city"),
      begin = m.begin,
      end = m.end,
      text = m.text
    )
  }

  import fr.dcram.nlp4s.automata.AutomatonFactory.seq
  rule("one-line")(
    (%("num")(StreetNum) | seq(Unit, %("num")(UnitNum))),
    Sep?,
    (%("streetName")(Capped.mn(1,3)) ~> %("roadType")(RoadType) | %("morphoRoadType")(MorphoRoadType)),
    (Sep ~> Capped.mn(1,3)).zeroN(2),
    Sep?,
    %("city")(City), %("zip")(UkPostcode)
  )
}
