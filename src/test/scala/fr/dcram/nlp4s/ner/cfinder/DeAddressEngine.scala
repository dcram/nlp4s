package fr.dcram.nlp4s.ner.cfinder

import fr.dcram.nlp4s.model.Token
import fr.dcram.nlp4s.ner._
import fr.dcram.nlp4s.tokenizer.Tokenizer

class DeAddressEngine extends NerEngine[NerAddress] {
  import fr.dcram.nlp4s.automata.AutomatonFactory._

  override def tokenizer: Tokenizer = Tokenizer("""[(){}.,!?;:]|(?:[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+(?:-[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+){0,3}'?)|[-]""".r)

  private val roadTypes = NerResource.asMap("resource://de/address-street-types.map", sep = '\t').map{case (k,v) => (k.lower, v)}

  private object MorphoStrasse extends NerTokenMatcher {
    def split(str:String):Option[(String,String)] = {
      if(str.length <= 6)
        None
      else
        (3 to str.length-3).toStream
          .map(str.splitAt)
          .find{case (_,part2) => roadTypes.contains(part2.lower)}
    }
    override def matches(tok: Token): Boolean = split(tok.text).nonEmpty
  }
  private val GermanProps = SetMatcher("in", "an", "zu", "im", "am").lower ~> SetMatcher("der", "dem", "die", "das", "den").lower.?
  private object Capped extends TxtMatcher(_.charAt(0).isUpper)
  private val City = seq(Capped, (GermanProps ~> Capped).?)
  private val Zip = RegexMatcher("""\d+""".r)
  private val RoadType: AbstractSetMatcher = SetMatcher(roadTypes.keys.toSeq: _*).lower
  private val Strasse1 = %("morphoStrasse")(GermanProps.?, Capped.?, MorphoStrasse, ".".?)
  private val Strasse2 = seq(%("streetName")(GermanProps.?, Capped.mn(1,2)) ~> %("streetType")(RoadType), ".".?)
  private object Sep extends SetMatcher(",", ".", "-", "―", "—", "–", "‒", "-",  "/")


  override def toNameEntity(m: NerMatch): NerAddress = {
    val (roadOpt:Option[String], streetNameOpt:Option[String]) = m.textOpt("streetType") match {
      case Some(stype) =>
        (roadTypes.get(stype.lower), m.textOpt("streetName"))
      case None =>
        m.textOpt("morphoStrasse")
          .flatMap(MorphoStrasse.split)
          .collect{
            case (sname, stype) =>
              (roadTypes.get(stype.lower), Some(sname))
          }.getOrElse((None, None))

    }
    NerAddress(
      num = m.textOpt("num"),
      streetType = roadOpt,
      streetName = streetNameOpt,
      zip = m.text("zip"),
      city = m.text("city"),
      begin = m.begin,
      end = m.end,
      text = m.text,
    )
  }
  rule("one-line")(
    Strasse1 | Strasse2, ("Nr" ~> ".".?).?, %("num")("""^\d+([-]\d+)?[A-Z]?$""".r ~> "[A-K]".r.?),
    Sep?,
    %("zip")(Zip), %("city")(City)
  )
}
