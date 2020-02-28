package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.model.Token
import fr.dcram.nlp4s.automata._
import scala.collection.mutable

trait NerEngine[NEType] {

  private[this] val rules:mutable.ListBuffer[(String, Transitionable[Token])] = new mutable.ListBuffer[(String, Transitionable[Token])]

  def rule(name:String)(s:Transitionable[Token]*):Unit = {
    require(s.length > 0, s"Empty list of transitionables")
    require(!ruleNames.contains(name), s"Rule already defined: ${name}")
    rules.append((name, AutomatonFactory.named(name, s:_*)))
  }

  private def ruleNames = rules.map(_._1).toSet


  def toNameEntity(m:RegexMatch[Token]):NEType
  def findAllMatchesIn(seq:Seq[Token]):Iterable[NEType] = {

    val automaton = AutomatonFactory.asAutomaton(AutomatonFactory.or(rules.map(_._2):_*))
    seqMatch(automaton, seq)
      .map(_.groups.toSeq.head)
      .flatMap {case (ruleName, matches) => matches}
      .map(toNameEntity)

  }

}
