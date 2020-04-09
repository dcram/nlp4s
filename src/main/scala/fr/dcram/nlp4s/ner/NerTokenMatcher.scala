package fr.dcram.nlp4s.ner
import fr.dcram.nlp4s.automata
import fr.dcram.nlp4s.automata.{Automaton, AutomatonBuilderDsl, Transitionable, UserTokenMatcher}
import fr.dcram.nlp4s.model.Token

abstract class NerTokenMatcher extends UserTokenMatcher[Token] with AutomatonBuilderDsl[Token]

abstract class NerAutomatonMatcher extends Transitionable[Token] with AutomatonBuilderDsl[Token] {
  def asAutomaton:Automaton[Token]
  override def asTransition(target: automata.State[Token]): automata.Transition[Token] = asAutomaton.asTransition(target)
}
