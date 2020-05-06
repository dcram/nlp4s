package fr.dcram.tokenizer

import fr.dcram.nlp4s.ner.Tokenizer
import org.scalatest.FunSpec

class TokenizerSpec extends FunSpec {

  describe(classOf[Tokenizer].toString) {
    describe("tokenize") {
      describe("indexing") {
        implicit val tokenizer = Tokenizer("""[\(\)\{\}\.,!\?;\:]|(?:[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+(?:-[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+){0,3}'?)|[-]""".r)
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
              assert(tokenizer.tokenize(string).map(tok => (tok.obj, tok.begin, tok.end)).toSeq == tokens)
          }
        }
      }

      def assertTok(string:String, expTokens: Iterable[String])(implicit tokenizer:Tokenizer):Unit = {
        assert(tokenizer.tokenize(string).map(_.obj).toIterable == expTokens)
      }
      describe("fr") {
        implicit val tokenizer = Tokenizer("""[\(\)\{\}\.,!\?;\:]|(?:[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+(?:-[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+){0,3}'?)|[-]""".r)
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
        implicit val  tokenizer: Tokenizer = Tokenizer("""[\(\)\{\}\.,!\?;\:]|(?:['\w]+(?:-\w+)?)|[-]""".r)

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
