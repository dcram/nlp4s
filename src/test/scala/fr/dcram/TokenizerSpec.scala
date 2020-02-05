package fr.dcram

import fr.dcram.nlp4s.Nlp4s
import fr.dcram.nlp4s.tokenizer.Tokenizer
import org.scalatest.FunSpec

class TokenizerSpec extends FunSpec {

  describe(classOf[Tokenizer].toString) {
    describe("tokenize") {
      describe("indexing") {
        val tokenizer = Nlp4s.tokenizer("fr")
        Seq(
          (
            "Bonjour Damien",
            Seq(("Bonjour", 0, 7), ("Damien", 8,14))
          ),
          (
            "C'est top! Bonjour, Damien.",
            Seq(("C'", 0,2), ("est", 2, 5), ("top", 6,9), ("!",9, 10), ("Bonjour",11, 18), (",",18, 19), ("Damien",20, 26), (".",26, 27))
          )
        ).foreach {
          case (string, tokens) =>
            it(s"should tokenize ${string}") {
              assert(tokenizer.tokenize(string).map(tok => (tok.text, tok.begin, tok.end)).toSeq == tokens)
          }
        }
      }

      def assertTok(string:String, expTokens: Iterable[String])(implicit tokenizer:Tokenizer):Unit = {
        assert(tokenizer.tokenize(string).map(_.text).toIterable == expTokens)
      }
      describe("fr") {
        implicit val tokenizer = Nlp4s.tokenizer("fr")
        Seq(
          (
            "Bonjour Damien",
            Seq("Bonjour", "Damien")
          ),
          (
            "C'est top! Bonjour, Damien.",
            Seq("C'", "est", "top", "!", "Bonjour", ",", "Damien", ".")
          )
        ).foreach { case (string, tokens) => it(s"should tokenize ${string}") {assertTok(string, tokens)}}
      }
      describe("uk") {
        implicit val tokenizer = Nlp4s.tokenizer("uk")
        Seq(
          (
            "Hello John",
            Seq("Hello", "John")
          ),
          (
            "That is John's",
            Seq("That", "is", "John's")
          )
        ).foreach { case (string, tokens) => it(s"should tokenize ${string}") {assertTok(string, tokens)}}
      }
    }
  }
}
