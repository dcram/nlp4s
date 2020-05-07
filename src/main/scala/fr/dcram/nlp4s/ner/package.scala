package fr.dcram.nlp4s

import fr.dcram.nlp4s.automata.AutomataReferenceTypes

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.matching.Regex

package object ner {

  type TokenParser[+A] = AutomataReferenceTypes.Parser[Token[String], Token[A]]
  type StringTokenizer = String => Stream[Token[String]]

  object TokenizerRef extends Tokenizers
  def regexTokenizer(regex:Regex):StringTokenizer = string => regex.findAllMatchIn(string).map(m => Token(m.start, m.end, m.group(0))).toStream

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
