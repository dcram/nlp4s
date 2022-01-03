package fr.dcram.nlp4s.spec

import fr.dcram.nlp4s.ner.NerTypes.StringTokenizer
import fr.dcram.nlp4s.ner._
import org.scalatest.funspec.AnyFunSpec

class TokenizerSpec extends AnyFunSpec {
  val wordTokenizer = regexTokenizer("""[\(\)\{\}\.,!\?;\:]|(?:[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+(?:-[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+){0,3}'?)|[-]""".r)

  describe(classOf[Tokenizers].toString) {
    describe("chain") {
      val sentenceTokenizer = regexTokenizer("""[^\.]*\.""".r)
      val sent1 = "Bonjour Toto."
      val sent2 = "Comment ça va."
      val sentence = s"$sent1 $sent2"
      import TokenizerRef._
      val chainedTokenizer = sentenceTokenizer.chain(wordTokenizer)

      it(s"""should tokenizer sentences in "$sentence"""") {
        assert(sentenceTokenizer(sentence) == Stream(Token(0,13,sent1),Token(13,28,s" $sent2")))
      }
      it(s"""should tokenizer words "${sentence}"""") {
        assert(wordTokenizer(sent2) == Stream(Token(0,7,"Comment"), Token(8,10,"ça"), Token(11,13,"va"), Token(13,14,".")))
      }
      it(s"""should tokenizer words in chain in sentence \"${sentence}\"""") {
        assert(chainedTokenizer(sentence) == Stream(
          Token(0,7,"Bonjour"),
          Token(8,12,"Toto"),
          Token(12,13,"."),
          Token(14,21,"Comment"),
          Token(22,24,"ça"),
          Token(25,27,"va"),
          Token(27,28,"."),
        ))
      }
    }
    describe("regexTokenizer") {
      describe("indexing") {
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
              assert(wordTokenizer(string).map(tok => (tok.obj, tok.begin, tok.end)).toSeq == tokens)
          }
        }
      }

      def assertTok(tokenizer:StringTokenizer)(string:String, expTokens: Iterable[String]):Unit = {
        assert(tokenizer(string).map(_.obj) == expTokens)
      }
      describe("fr") {
        implicit val tokenizer = regexTokenizer("""[\(\)\{\}\.,!\?;\:]|(?:[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+(?:-[\wßçÇÀàéèÉÈùÙÊêîÎûÛÔôäëïöüÄËÏÖÜ]+){0,3}'?)|[-]""".r)
        Seq(
          (
            "Bonjour Damien",
            Seq("Bonjour", "Damien")
          ),
          (
            "C'est top! Bonjour, Damien.",
            Seq("C'", "est", "top", "!", "Bonjour", ",", "Damien", ".")
          )
        ).foreach { case (string, tokens) => it(s"should tokenize ${string}") {assertTok(tokenizer)(string, tokens)}}
      }
      describe("uk") {
        implicit val tokenizer = regexTokenizer("""[\(\)\{\}\.,!\?;\:]|(?:['\w]+(?:-\w+)?)|[-]""".r)

        Seq(
          (
            "Hello John",
            Seq("Hello", "John")
          ),
          (
            "That is John's",
            Seq("That", "is", "John's")
          )
        ).foreach { case (string, tokens) => it(s"should tokenize ${string}") {assertTok(tokenizer)(string, tokens)}}
      }
    }
  }
}
