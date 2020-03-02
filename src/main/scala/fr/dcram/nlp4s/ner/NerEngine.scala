package fr.dcram.nlp4s.ner

import fr.dcram.nlp4s.automata.{AutomatonBuilderDsl, _}
import fr.dcram.nlp4s.model.Token
import fr.dcram.nlp4s.tokenizer.Tokenizer

import scala.collection.mutable

trait NerEngine[NEType]  {


  implicit class TransitionableWithDsl[Tok](t:Transitionable[Tok]) extends Transitionable[Tok] with AutomatonBuilderDsl[Tok] {
    override def asTransition(target: State[Tok]): Transition[Tok] = t.asTransition(target)
  }

  private[this] val rules:mutable.ListBuffer[(String, Transitionable[Token])] = new mutable.ListBuffer[(String, Transitionable[Token])]
  def %[Tok](name:String)(a:Transitionable[Tok]):Transitionable[Tok]  = AutomatonFactory.named(name, a)

  def rule(name:String)(s:Transitionable[Token]*):Unit = {
    require(s.nonEmpty, s"Empty list of transitionables")
    require(!ruleNames.contains(name), s"Rule already defined: $name")
    rules.append((name, AutomatonFactory.named(name, s:_*)))
  }

  private def ruleNames = rules.map(_._1).toSet


  def toNameEntity(m:NerMatch):NEType
  def tokenizer:Tokenizer
  private[this] lazy val _tokenizer = tokenizer
  def findAllMatchesIn(srcStr:String):Iterable[NEType] = {
    val seq = _tokenizer.tokenize(srcStr)
    val automaton = AutomatonFactory.asAutomaton(AutomatonFactory.or(rules.map(_._2):_*))
    seqMatch(automaton, seq)
      .map(_.groups.toSeq.head)
      .flatMap {case (ruleName, matches) => matches}
      .map(rm => NerMatch.apply(rm, srcStr))
      .map(toNameEntity)

  }

}
