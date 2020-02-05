package fr.dcram.nlp4s.tokenizer

import fr.dcram.nlp4s.model.Token

class Tokenizer(resource:TokenizerResource) {
  def tokenize(string:String):Stream[Token] = {
    resource.splitRegex.findAllMatchIn(string).map(m => Token(m.start, m.end, m.group(0)))
  }.toStream
}
