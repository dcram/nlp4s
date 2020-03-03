package fr.dcram.nlp4s.ner

import java.net.URI

import fr.dcram.nlp4s.tokenizer.Tokenizer

import scala.io.Source

object NerResource {
  def source(uri:String) = {
    val parsed = new URI(uri)
    val src = Option(parsed.getScheme).map(_.toLowerCase) match {
      case Some("file") => Source.fromFile(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
      case Some("resource") =>
        Source.fromResource(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
      case Some(protocol) => throw new UnsupportedOperationException(s"Unsupported protocol: $protocol")
      case None => Source.fromFile(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
    }
    src
  }

  def asMap(uri:String, sep:Char):Map[String, String] = {
    source(uri).getLines().map(_.trim)
      .filterNot(_.startsWith("#"))
      .map(_.split(sep))
      .filter(_.length > 1)
      .map(m => m(0) -> m(1))
      .toMap
  }
  def asTrie(uri:String, sep:Char)(implicit tokenizer:Tokenizer):Trie[String, Iterable[String]] = {
    val map = asMap(uri, sep)
    Trie(map.map{case(k,v) => (tokenizer.tokenize(k).map(_.text), v)})
  }
  def asSet(uri:String):Set[String] = {
    source(uri).getLines().map(_.trim)
      .filterNot(_.startsWith("#"))
      .toSet
  }
}
