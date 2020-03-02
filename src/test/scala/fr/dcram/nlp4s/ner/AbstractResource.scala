package fr.dcram.nlp4s.ner

import java.net.URI

import scala.io.Source

class AbstractResource(uri:String) {
  private val parsed = new URI(uri)
  protected val source = Option(parsed.getScheme).map(_.toLowerCase) match {
    case Some("file") => Source.fromFile(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
    case Some("resource") =>
      Source.fromResource(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
    case Some(protocol) => throw new UnsupportedOperationException(s"Unsupported protocol: $protocol")
    case None => Source.fromFile(parsed.getRawSchemeSpecificPart.replaceFirst("^//", ""))
  }
}
