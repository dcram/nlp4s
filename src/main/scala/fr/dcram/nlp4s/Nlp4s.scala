package fr.dcram.nlp4s

import fr.dcram.nlp4s.tokenizer.{Tokenizer, TokenizerResource}

import scala.io.Source

object Nlp4s {

  def tokenizer(lang:String):Tokenizer = {
    val resourcePath = s"/nlp/$lang/tokenizer.regex"
    val inputStream = getClass.getResourceAsStream(resourcePath)
    if(inputStream == null)
      throw new IllegalArgumentException(s"No such resource $resourcePath")
    val regex = Source.fromInputStream(inputStream).mkString.trim
    new Tokenizer(TokenizerResource(splitRegex = regex.r))
  }
}
