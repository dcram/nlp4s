package fr.dcram.nlp4s.ner

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.FunSpec

import scala.language.implicitConversions

class AddressEngineSpec extends FunSpec with LazyLogging {
  val tokenizer: Tokenizer = Tokenizer("""[\(\)\{\}\.,!\?;\:]|(?:['\w]+(?:-\w+)?)|[-]""".r)

  private val streetTypes = NerResource.asMap("resource://fr/street-types.map", sep = ',')

  case class Address(
                      num:Option[String],
                      streetType:Option[String],
                      streetName:Option[String],
                      zip:String,
                      city:String,
                      override val begin:Int,
                      override val end:Int,
                      override val text:String) extends Annotation


  def addressParser(P:TokenParsers):TokenParser[Address] = {
    import P._

    val streetType:TokenParser[String] = (inMap(streetTypes) ~ str(".").opt).map(_._1)
    val zip:TokenParser[String] = digit(5) or (digit(2) ~ digit(3)).map{case (a,b) => a+b}
    val city:TokenParser[String] = $(_.charAt(0).isUpper)
    val sep:TokenParser[String] = inSet(Set(",", "-"))
    val streetName:TokenParser[String] = reg("""^[\w-']+$""".r).map(_.group(0))

    seq(digit, streetType, streetName.mn(1,8), sep.opt, zip, city).map {
      case (num, stype, sname, _, zip, city) => Address(
        Some(num),
        Some(stype),
        Some(sname.mkString(" ")),
        zip,
        city,
        0,0,""
      )
    }
  }

  describe(classOf[TokenParsers].toString) {
    Seq(
      ("10 rue Paul Blanchard , 44000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rue Paul Blanchard 44000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 av Paul Blanchard 44000 Nantes", Some(Some("10"), Some("avenue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 av. Paul Blanchard 44000 Nantes", Some(Some("10"), Some("avenue"), Some("Paul Blanchard"), "44000", "Nantes")),
      ("10 rue Paul Blanchard 44 000 Nantes", Some(Some("10"), Some("rue"), Some("Paul Blanchard"), "44000", "Nantes"))
    ).foreach {
      case (sentence, Some(address@(num, streetType, streetName, zip, city))) =>
        describe(sentence) {
          val tokens = tokenizer.tokenize(sentence)
          object Ref extends TokenParsers
          val addresses:List[Address] = Ref.scan(addressParser(Ref))(tokens).toList
          it("should extract one address") {assert(addresses.length == 1)}
          addresses.headOption.foreach{
            addr =>
              it(s"should extract num ${num}") {assert(addr.num == num)}
              it(s"should extract streetType ${streetType}") {assert(addr.streetType == streetType)}
              it(s"should extract streetName ${streetName}") {assert(addr.streetName == streetName)}
              it(s"should extract zip ${zip}") {assert(addr.zip == zip)}
              it(s"should extract city ${city}") {assert(addr.city == city)}

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
    object Ref extends TokenParsers

    it("bm1") {
      val tokens = tokenizer.tokenize("10 rue Paul Blanchard , 44000 Nantes")

      for(n <- Seq(1,10,100,1000,10000,100000)) {
        val duration = bm(n, () => {
          val addresses:Option[Address] = Ref.scan(addressParser(Ref))(tokens).headOption
        })
        println(f"$n%8d times -> $duration%d ms")
      }
    }
    val n = 100000
    val sent = "10 rue Paul Blanchard , 44000 Nantes. C'est 3 rue rien. "
    it(s"should not throw StackTraceOverflow when scanning addresses from sentence repeated $n times") {
      val sent10000 = sent * n

      logger.info(s"tokenizing the sentence ${sent} * $n")
      val tokens = tokenizer.tokenize(sent10000)

      logger.info(s"scanning addresses")
      val addresses:List[Address] = Ref.scan(addressParser(Ref))(tokens).toList
      logger.info(s"scanned ${addresses.size} addresses")
      assert(addresses.size == n)

    }
  }
}
