package fr.dcram.nlp4s.spec

import com.typesafe.scalalogging.LazyLogging
import fr.dcram.nlp4s.ner.NerTypes.TokenParser
import fr.dcram.nlp4s.ner.{Token, TokenParsers, TokenParsersRef, regexTokenizer}
import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

class TokenParsersSpec extends AnyFunSpec with LazyLogging {
  val tokenizer = regexTokenizer("""[(){}.,!?;:]|(?:['\w]+(?:-\w+)?)|[-]""".r)

  import TokenParsersRef._
  private val streetTypes = loadTrie("resource://fr/street-types.trie", sep = ',', tokenizer = tokenizer)

  case class Address(
                      num:Option[String],
                      streetType:Option[String],
                      streetName:Option[String],
                      zip:String,
                      city:String)


  val zipCode:TokenParser[String] = digit(5) or (digit(2) ~ digit(3)).map{case (s1,s2) => s"$s1$s2" }

  val addressParser:TokenParser[Address] = {
    val streetType:TokenParser[String] = (in(streetTypes).lower_() ~ ".".opt).map(_._1)
    val city:TokenParser[String] = ###(_.charAt(0).isUpper)
    val sep:TokenParser[String] = in(",", "-")
    val streetName:TokenParser[String] = """^[\w-']+$""".r.map(_.group(0))

    $(digit, streetType, streetName.mn(1,8), sep.opt, zipCode, city)
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

    describe(classOf[NerResource[_]].toString) {
      describe("preparation") {

        val sentence = tokenizer("a b A b A B c")
        Seq(
          (in("a"), Seq(("a", 0, 1))),
          (in("a").lower_(), Seq(("a", 0, 1), ("A", 4, 5), ("A", 8, 9))),
          (in("A"), Seq(("A", 4, 5), ("A", 8, 9))),
          (in("A").lower_(), Seq(("a", 0, 1), ("A", 4, 5), ("A", 8, 9))),
        ).zipWithIndex.foreach {
          case ((parser, tokens), i) =>
            it(s"$i. should extract tokens ${tokens}") {
              assert(parser.scan(sentence).matchList.map(_.tokens.head).map{case Token(begin, end, txt) => (txt, begin, end)} == tokens)
            }
        }
      }
    }

    describe("Preservation of offsets") {
      val sent = "10 rue Paul Blanchard , 44 000 Nantes"
      val tokens = tokenizer(sent)
      val token:Token[Address] = addressParser.scan(tokens).matchList.head.data

      describe("full sentence") {
        it("begin=0"){assert(token.begin == 0)}
        it(s"end=37"){assert(token.end == 37)}
      }

      describe("zip") {
        val token:Token[String] = zipCode.scan(tokens).matchList.head.data
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
          val addresses:List[Token[Address]] = addressParser.scan(tokens).matchList.map(_.data).toList
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
      import scala.language.reflectiveCalls
      n.times(f())
      val e = System.currentTimeMillis()
      e-s
    }

    it("bm1") {
      val tokens = tokenizer("10 rue Paul Blanchard , 44000 Nantes")

      for(n <- Seq(1,10,100,1000,10000)) {
        val duration = bm(n, () => {
          val addresses:Option[Token[Address]] = addressParser.scan(tokens).matchList.headOption.map(_.data)
        })
        println(f"$n%8d times -> $duration%d ms")
      }
    }
    val timeoutMillis=100L
    it(s"bm2 (with timeout=${timeoutMillis}ms)") {

      for(n <- Seq(1,10,100,1000,10000,100000,1000000)) {
        val tokens = tokenizer("10 rue Paul Blanchard , 44000 Nantes. ".repeat(n))
        val start = System.currentTimeMillis()
        val size = addressParser.scan(tokens, timeoutMillis).matchList.size
        val duration = System.currentTimeMillis() - start
        println(f"$n%8d times -> found $size matches in $duration%d ms")
      }
    }
    val matchLimit=17
    it(s"bm3 (with matchLimit=${matchLimit})") {
      for(n <- Seq(1,10,100,1000,10000,100000,1000000)) {
        val tokens = tokenizer("10 rue Paul Blanchard , 44000 Nantes. ".repeat(n))
        val start = System.currentTimeMillis()
        val value = addressParser.scan(tokens, timeoutMillis = Long.MaxValue, matchLimit = matchLimit)
        val size = value.matchList.size
        val duration = System.currentTimeMillis() - start
        println(f"$n%8d times -> found $size matches in $duration%d ms")
        if(value.matchLimitReached) {
          assert(size == matchLimit)
        } else {
          assert(size <= matchLimit)
        }
      }
    }
    val n = 100000
    val sent = "10 rue Paul Blanchard , 44000 Nantes. C'est 3 rue rien. "
    it(s"should not throw StackTraceOverflow when scanning addresses from sentence repeated $n times") {
      val sent10000 = sent * n

      logger.info(s"tokenizing the sentence ${sent} * $n")
      val tokens = tokenizer(sent10000)

      logger.info(s"Num tokens: ${tokens.size}")

      logger.info(s"scanning addresses")
      val addresses:List[Token[Address]] = addressParser.scan(tokens).matchList.map(_.data).toList
      logger.info(s"scanned ${addresses.size} addresses")
      assert(addresses.size == n)

    }
  }
}
