package fr.dcram.nlp4s

import fr.dcram.nlp4s.automata.{AutomataReference, AutomataReferenceTypes}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.matching.Regex

package object ner {
  type TokenParser[+A] = AutomataReferenceTypes.Parser[Token, A]

  trait TokenParsers extends AutomataReference[Token] {
    type TP[+A] = TokenParser[A]
    def $(f:String => Boolean):TokenParser[String] = tok(t => f(t.text)).map(_.text)

    def digit:TokenParser[String] = reg("""^\d+$""".r).map(_.group(0))
    def digit(n:Int):TokenParser[String] = reg(s"^\\d{$n}$$".r).map(_.group(0))
    def inSet(strings:Set[String]):TokenParser[String] = tokA(t => if(strings.contains(t.text)) Some(t.text) else None)
    def inMap[V](map:Map[String, V]):TokenParser[V] = tokA(t => map.get(t.text))

    def or[A](p1:TP[A], p2: => TP[A], p3: => TP[A]):TP[A] = p1 or p2 or p3
    def or[A](p1:TP[A], p2: => TP[A], p3: => TP[A], p4: => TP[A]):TP[A] = p1 or p2 or p3 or p4
    def or[A](p1:TP[A], p2: => TP[A], p3: => TP[A], p4: => TP[A], p5: => TP[A]):TP[A] = p1 or p2 or p3 or p4 or p5
    def or[A](p1:TP[A], p2: => TP[A], p3: => TP[A], p4: => TP[A], p5: => TP[A], p6: => TP[A]):TP[A] = p1 or p2 or p3 or p4 or p5 or p6
    def or[A](p1:TP[A], p2: => TP[A], p3: => TP[A], p4: => TP[A], p5: => TP[A], p6: => TP[A], p7: => TP[A]):TP[A] = p1 or p2 or p3 or p4 or p5 or p6 or p7
    def or[A](p1:TP[A], p2: => TP[A], p3: => TP[A], p4: => TP[A], p5: => TP[A], p6: => TP[A], p7: => TP[A], p8: => TP[A]):TP[A] = p1 or p2 or p3 or p4 or p5 or p6 or p7 or p8

    def seq[A1,A2,A3](p1:TP[A1], p2: TP[A2], p3: TP[A3]):TP[(A1,A2,A3)] = (p1 ~ p2 ~ p3).map{case ((a1,a2),a3) => (a1, a2, a3)}
    def seq[A1,A2,A3,A4](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4]):TP[(A1,A2,A3,A4)] = (p1 ~ p2 ~ p3 ~ p4).map{case (((a1,a2),a3),a4) => (a1, a2, a3, a4)}
    def seq[A1,A2,A3,A4,A5](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4], p5: TP[A5]):TP[(A1,A2,A3,A4,A5)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5).map{case ((((a1,a2),a3),a4),a5) => (a1, a2, a3, a4, a5)}
    def seq[A1,A2,A3,A4,A5,A6](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4], p5: TP[A5], p6: TP[A6]):TP[(A1,A2,A3,A4,A5,A6)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6).map{case (((((a1,a2),a3),a4),a5),a6) => (a1, a2, a3, a4, a5, a6)}
    def seq[A1,A2,A3,A4,A5,A6,A7](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4], p5: TP[A5], p6: TP[A6], p7: TP[A7]):TP[(A1,A2,A3,A4,A5,A6,A7)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7).map{case ((((((a1,a2),a3),a4),a5),a6),a7) => (a1, a2, a3, a4, a5, a6, a7)}
    def seq[A1,A2,A3,A4,A5,A6,A7,A8](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4], p5: TP[A5], p6: TP[A6], p7: TP[A7], p8: TP[A8]):TP[(A1,A2,A3,A4,A5,A6,A7,A8)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8).map{case (((((((a1,a2),a3),a4),a5),a6),a7),a8) => (a1, a2, a3, a4, a5, a6, a7, a8)}
    def seq[A1,A2,A3,A4,A5,A6,A7,A8,A9](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4], p5: TP[A5], p6: TP[A6], p7: TP[A7], p8: TP[A8], p9: TP[A9]):TP[(A1,A2,A3,A4,A5,A6,A7,A8,A9)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8 ~ p9).map{case ((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9) => (a1, a2, a3, a4, a5, a6, a7, a8, a9)}
    def seq[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](p1:TP[A1], p2: TP[A2], p3: TP[A3], p4: TP[A4], p5: TP[A5], p6: TP[A6], p7: TP[A7], p8: TP[A8], p9: TP[A9], p10: TP[A10]):TP[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)] = (p1 ~ p2 ~ p3 ~ p4 ~ p5 ~ p6 ~ p7 ~ p8 ~ p9 ~ p10).map{case (((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9),a10) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)}

    implicit def reg(r:Regex):TokenParser[Regex.Match] = tokA(t => r.findFirstMatchIn(t.text))
    implicit def str(str:String):TokenParser[String] = tokA(t => if(t.text == str) Some(str) else None)

  }

  private[this] lazy val AccentsFromChars = "ßÀÁÂÃÄÅàáâãäåĀāĂăĄąÇçĆćĈĉĊċČčÐðĎďĐđÈÉÊËèéêëĒēĔĕĖėĘęĚěĜĝĞğĠġĢ‌​ģĤĥĦħÌÍÎÏìíîïĨĩĪīĬĭĮ‌​įİıĴĵĶķĸĹĺĻļĽľĿŀŁłÑñ‌​ŃńŅņŇňŉŊŋÒÓÔÕÖØòóôõö‌​øŌōŎŏŐőŔŕŖŗŘřŚśŜŝŞşŠ‌​šȘșſŢţŤťŦŧȚțÙÚÛÜùúûü‌​ŨũŪūŬŭŮůŰűŲųŴŵÝýÿŶŷŸ‌​ŹźŻżŽž"
  private[this] lazy val AccentsToChars   = "sAAAAAAaaaaaaAaAaAaCcCcCcCcCcDdDdDdEEEEeeeeEeEeEeEeEeGgGgGgG‌​gHhHhIIIIiiiiIiIiIiI‌​iIiJjKkkLlLlLlLlLlNn‌​NnNnNnnNnOOOOOOooooo‌​oOoOoOoRrRrRrSsSsSsS‌​sSssTtTtTtTtUUUUuuuu‌​UuUuUuUuUuUuWwYyyYyY‌​ZzZzZz"

  private[this] lazy val AccentMap:Map[Char, Char] = {
    val map = new mutable.HashMap[Char, Char]
    (0 until AccentsFromChars.length).foreach(i => {
      map.put(AccentsFromChars.charAt(i), AccentsToChars.charAt(i))
    })
    map.toMap
  }


  implicit class StringImpr(s:String) {
    def lower:String = s.toLowerCase
    def ascii:String =   s.map(c=> AccentMap.getOrElse(c, c))
    def upper:String = s.toUpperCase
    def capped:Boolean = s.charAt(0).isUpper
    def isLower:Boolean = s.forall(_.isLower)
    def isUpper:Boolean = s.forall(_.isUpper)
  }
}
