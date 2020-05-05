package fr.dcram.nlp4s.ner

import java.net.URI

import fr.dcram.nlp4s.data.Trie

import scala.io.Source

object NerResource {
  def source(uri:String):Source = {
    val parsed = new URI(uri)
    val src = Option(parsed.getScheme).map(_.toLowerCase) match {
      case Some("file") => Source.fromFile(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
      case Some("resource") =>
        Source.fromInputStream(getClass.getResourceAsStream(parsed.getRawSchemeSpecificPart.replaceFirst("^//", "/")))
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

  def asTrie(uri:String, sep:Char, tokenizer:String => Iterable[Token], tokPreparator:Token => String):Trie[String, Iterable[String], Token] = {
    val map = asMap(uri, sep)
    Trie.fromEntries[String, String, Token](
      entries = map.map{case(k,v) => (tokenizer(k).map(tokPreparator).toSeq, v)},
      tokPreparator = tokPreparator
    )
  }

  def asSet(uri:String):Set[String] = {
    source(uri).getLines().map(_.trim)
      .filterNot(_.startsWith("#"))
      .toSet
  }
}
