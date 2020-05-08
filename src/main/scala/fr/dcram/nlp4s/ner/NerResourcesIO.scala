package fr.dcram.nlp4s.ner

import java.net.URI

import fr.dcram.nlp4s.ner.NerTypes.StringTokenizer
import fr.dcram.nlp4s.util.Trie

import scala.io.Source

trait NerResourcesIO {
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

  def loadMap(uri:String, sep:Char):Map[String, String] = {
    source(uri).getLines().map(_.trim)
      .filterNot(_.startsWith("#"))
      .map(_.split(sep))
      .filter(_.length > 1)
      .map(m => m(0) -> m(1))
      .toMap
  }

  def loadTrie(uri:String, sep:Char, tokenizer:StringTokenizer):Trie[String, String] = {
    val map = loadMap(uri, sep)
    Trie.fromEntries[String, String](
      entries = map.map{case(k,v) => (tokenizer(k).map(_.obj), v)}
    )
  }

  def loadSet(uri:String):Set[String] = {
    source(uri).getLines().map(_.trim)
      .filterNot(_.startsWith("#"))
      .toSet
  }
}
