package sandbox

import java.io.PrintWriter

import com.typesafe.scalalogging.LazyLogging

import scala.io.Source

object ExtractDeSuffixesApp extends App with LazyLogging {
  case class Address(street:String, city:String, zip:String)
  private val file = sys.env("ADDRESSES_FILE")
  logger.info(s"Reading addresses at $file")
  val addresses = Source.fromFile(file).getLines()
    .map(_.split(";"))
    .map(s => Address(s(0), s(1), s(2)))
    .drop(1)
    .take(100000)
    .toSeq

  logger.info(s"n addresses: ${addresses.size}")

  addresses.take(10).foreach(println)
  val pw = new PrintWriter(s"$file.street-types.txt")
  val Digit = """^\d+""".r
  private val moreThan2Words = addresses
    .to(LazyList)
    .map(_.street.split("\\s+")
    .filterNot(s => Digit.findFirstMatchIn(s).isDefined).toList)
    .filter(_.length > 1)
  val suffixes = moreThan2Words
    .map(_.last)
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .toSeq
    .sortBy(-_._2)
    .filter(_._1.length > 2)


  val nGt2 = moreThan2Words.size

  val masterSuffixes = suffixes.foldLeft(Seq.empty[String]) {
    case (knowSuffixes, (suff, freq)) =>
      if(knowSuffixes.find(s => suff.toLowerCase.endsWith(s.toLowerCase())).isDefined)
        knowSuffixes
      else
        knowSuffixes :+ suff
  }

  masterSuffixes
    .map{case suff => s"${suff.toLowerCase}\t${suff}\n"}
    .foreach(pw.write)
  pw.close

}
