package fr.dcram.nlp4s.spec

import com.typesafe.scalalogging.LazyLogging
import fr.dcram.nlp4s.ner.NerTypes.TokenParser
import fr.dcram.nlp4s.ner.{NerResource, Token, TokenParsers, regexTokenizer}
import org.scalatest.FunSpec

import scala.language.implicitConversions

class TokenParsersSpec extends FunSpec with LazyLogging {
  val tokenizer = regexTokenizer("""[(){}.,!?;:]|(?:['\w]+(?:-\w+)?)|[-]""".r)

  private val streetTypes = NerResource.asTrie("resource://fr/street-types.trie", sep = ',', tokenizer = tokenizer)

  object Ref extends TokenParsers

  case class Address(
                      num:Option[String],
                      streetType:Option[String],
                      streetName:Option[String],
                      zip:String,
                      city:String)


  def zip(P:TokenParsers):TokenParser[String] = {
    import P._
    digit(5) or (digit(2) ~ digit(3)).map{case (s1,s2) => s"$s1$s2" }
  }

  def addressParser(P:TokenParsers):TokenParser[Address] = {
    import P._

    val streetType:TokenParser[String] = (inTrie(streetTypes) ~ ".".opt).map(_._1._1)
    val city:TokenParser[String] = ###(_.charAt(0).isUpper)
    val sep:TokenParser[String] = inSet(Set(",", "-"))
    val streetName:TokenParser[String] = """^[\w-']+$""".r.map(_.group(0))

    $(digit, streetType, streetName.mn(1,8), sep.opt, zip(P), city)
      .map {
        case (num, stype, sname, _, zip, city) => Address(
          Some(num),
          Some(stype),
          Some(sname.mkString(" ")),
          zip,
          city
        )
      }
  }

  describe(classOf[TokenParsers].toString) {

    describe("Preservation of offsets") {
      val sent = "10 rue Paul Blanchard , 44 000 Nantes"
      val tokens = tokenizer(sent)
      val token:Token[Address] = Ref.scan(addressParser(Ref))(tokens).head

      describe("full sentence") {
        it("begin=0"){assert(token.begin == 0)}
        it(s"end=37"){assert(token.end == 37)}
      }

      describe("zip") {
        val token:Token[String] = Ref.scan(zip(Ref))(tokens).head
        it("begin=24"){assert(token.begin == 24)}
        it(s"end=30"){assert(token.end == 30)}
      }
    }

    Seq(
      ("10 rue Paul Blanchard , 44000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rue Paul Blanchard 44000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 av Paul Blanchard 44000 Nantes", Some(Some("10"), Some("avenue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 av. Paul Blanchard 44000 Nantes", Some(Some("10"), Some("avenue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rue Paul Blanchard 44 000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rond point de Paris 44 000 Nantes", Some(Some("10"), Some("RP"), Some("de Paris"), "44000", "Nantes"))
    ).foreach {
      case (sentence, Some(address@(num, streetType, streetName, zip, city))) =>
        describe(sentence) {
          val tokens = tokenizer(sentence)
          val addresses:List[Token[Address]] = Ref.scan(addressParser(Ref))(tokens).toList
          it("should extract one address") {assert(addresses.length == 1)}
          addresses.headOption.foreach{
            addr =>
              it(s"should extract num $num") {assert(addr.obj.num == num)}
              it(s"should extract streetType $streetType") {assert(addr.obj.streetType == streetType)}
              it(s"should extract streetName $streetName") {assert(addr.obj.streetName == streetName)}
              it(s"should extract zip $zip") {assert(addr.obj.zip == zip)}
              it(s"should extract city $city") {assert(addr.obj.city == city)}

          }
        }
    }

    def bm(n:Int, f:()=>Unit):Long = {
      implicit def intWithTimes(n: Int) = new {
        def times(f: => Unit):Unit = 1 to n foreach {_ => f}
      }

      val s = System.currentTimeMillis()
      n.times(f())
      val e = System.currentTimeMillis()
      e-s
    }

    it("bm1") {
      val tokens = tokenizer("10 rue Paul Blanchard , 44000 Nantes")

      for(n <- Seq(1,10,100,1000,10000,100000)) {
        val duration = bm(n, () => {
          val addresses:Option[Token[Address]] = Ref.scan(addressParser(Ref))(tokens).headOption
        })
        println(f"$n%8d times -> $duration%d ms")
      }
    }
    val n = 100000
    val sent = "10 rue Paul Blanchard , 44000 Nantes. C'est 3 rue rien. "
    it(s"should not throw StackTraceOverflow when scanning addresses from sentence repeated $n times") {
      val sent10000 = sent * n

      logger.info(s"tokenizing the sentence ${sent} * $n")
      val tokens = tokenizer(sent10000)

      logger.info(s"scanning addresses")
      val addresses:List[Token[Address]] = Ref.scan(addressParser(Ref))(tokens).toList
      logger.info(s"scanned ${addresses.size} addresses")
      assert(addresses.size == n)

    }
  }
}
