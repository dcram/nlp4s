package fr.dcram.nlp4s

import fr.dcram.nlp4s.automata.{TokenMatcher, TransitionInstance}

import scala.annotation.tailrec

package object automata {

  trait Transitionable[Tok, TI<:TransitionInstance[Tok]] {
    def asTransition(target:State[Tok]):Transition[Tok, TI]
  }
  trait TokenMatcher[Tok] extends Transitionable[Tok, TokenMatcher[Tok]] {
    def matches(tok:Tok):Boolean

    override def asTransition(target: State[Tok]): Transition[Tok,TokenMatcher[Tok]] = new MatcherTransition[Tok](this, target)
  }

  abstract class Transition[Tok,TI<:TransitionInstance[Tok]](val target:State[Tok]) {
    def instantiate:TI
  }
  case class EpsilonTransition[Tok](override val target:State[Tok]) extends Transition[Tok,EpsilonTransition[Tok]](target) {
    override def toString: String = "Ɛ"
    override def instantiate: EpsilonTransition[Tok] = this
  }
  case class AutomatonTransition[Tok](name:Option[String], automaton:Automaton[Tok], override val target:State[Tok]) extends Transition[Tok, AutomatonInstance[Tok]](target) {
    override def toString: String = s"${name.map(s => s"@($s)").getOrElse("")}Automaton"
    override def instantiate: AutomatonInstance[Tok] = AutomatonInstance.instanciate(automaton)
  }
  case class MatcherTransition[Tok](matcher:TokenMatcher[Tok], override val target:State[Tok]) extends Transition[Tok](target) {
    override def toString: String = matcher.getClass.getSimpleName
  }
  case class State[Tok](transitions:List[Transition[Tok, _]], accepting:Boolean = false)  {
    override def toString: String = s"State(${accepting})"}
  abstract class Match[Tok, T <: Transition[Tok]](val transition: T)
  case class EpsilonMatch[Tok](override val transition:EpsilonTransition[Tok]) extends Match(transition) with TransitionInstance[Tok]
  case class TokenMatch[Tok](token:Tok, override val transition:MatcherTransition[Tok]) extends Match(transition) with TransitionInstance[Tok]
  case class AutomatonMatch[Tok](nameOpt:Option[String], i:AutomatonInstance[Tok], override val transition: AutomatonTransition[Tok]) extends Match(transition)

  case class AutomatonInstance[Tok](current: StateInstance[Tok], stateStack:List[(StateInstance[Tok], Match[Tok, _])], automatonStack:List[AutomatonInstance[Tok]], seq:Seq[Tok])  extends  TransitionInstance[Tok]
  case class StateInstance[Tok](state:State[Tok], rest:List[TransitionInstance[Tok]])  {
    override def toString: String = s"StateInstance(${state.accepting},${rest.map(_.toString).mkString("|")})"}

  trait TransitionInstance[Tok]
  object AutomatonInstance{
    def instanciate[Tok](seq:Seq[Tok],automaton:Automaton[Tok]):AutomatonInstance[Tok] = AutomatonInstance(
      StateInstance(automaton.initialState, automaton.initialState.transitions.map(_.instantiate)),
      List.empty,
      List.empty,
      seq
    )
  }


  private[this] def backtrack[Tok](i:AutomatonInstance[Tok]):Option[AutomatonInstance[Tok]] = i match {
    case AutomatonInstance(_, Nil, Nil, _) => None
    case AutomatonInstance(_, Nil, aStackHead :: _, _) => backtrack(aStackHead)
    case AutomatonInstance(_, (si, EpsilonMatch(_)) :: tStack, aStack, seq) => Some(AutomatonInstance(si, tStack, aStack, seq))
    case AutomatonInstance(_, (si, TokenMatch(tok, _)) :: tStack, aStack, seq) => Some(AutomatonInstance(si, tStack, aStack, tok +: seq))
    case AutomatonInstance(_, (si, AutomatonMatch(_, matchedI, _)) :: tStack, aStack, seq) =>
  }

  /*
  The reactor method of automaton instance matching
   */
  @tailrec
  private[this] def nextPrefixMatch[Tok](i:AutomatonInstance[Tok]):Option[AutomatonInstance[Tok]] = {
    i match {
      case AutomatonInstance(StateInstance(currentState, _), _, aStack, Nil) if currentState.accepting =>
        Some(i)
      case AutomatonInstance(StateInstance(_, _), _, aStack, Nil)  =>
        backtrack(i) match {
          case Some(bi) => nextPrefixMatch[Tok](bi)
          case None => None
        }
      case AutomatonInstance(StateInstance(currentState, Nil), _, aStack, _) if currentState.accepting =>
        Some(i)
      case AutomatonInstance(StateInstance(_, Nil), _, aStack, _) =>
        backtrack(i) match {
          case Some(bi) => nextPrefixMatch[Tok](bi)
          case None => None
        }
      case AutomatonInstance(StateInstance(currentState, EpsilonTransition(target) +: restTransitions), stack, aStack, s) =>
        nextPrefixMatch(AutomatonInstance(
          StateInstance(target, target.transitions),
          (StateInstance(currentState, restTransitions), EpsilonMatch(EpsilonTransition(target))) :: stack,
          aStack,
          s
        ))
      case AutomatonInstance(StateInstance(currentState, MatcherTransition(matcher, target) +: restTransitions), stack, aStack,  tok +: restSeq) if matcher.matches(tok) =>
        nextPrefixMatch(AutomatonInstance(
          StateInstance(target, target.transitions),
          (StateInstance(currentState, restTransitions), TokenMatch(tok, MatcherTransition(matcher, target))) :: stack,
          aStack,
          restSeq
        ))
      case AutomatonInstance(StateInstance(currentState, AutomatonTransition(nameOpt, automaton, target) +: restTransitions), tStack, aStack, seq) =>

      case AutomatonInstance(StateInstance(currentState, _ +: restTransitions), stack, aStack, seq) =>
        nextPrefixMatch(AutomatonInstance(
          StateInstance(currentState, restTransitions),
          stack,
          aStack,
          seq
        ))
    }
  }


  object Automaton {
    def empty[Tok]:Automaton[Tok] = Automaton[Tok](State(transitions = List.empty, accepting = true))
  }

  case class Automaton[Tok](initialState:State[Tok]) extends Transitionable[Tok] {
    override def asTransition(target: State[Tok]): Transition[Tok] = new AutomatonTransition[Tok](None, this, target)
    def seqMatch(sequence: Seq[Tok]): Seq[Seq[Match[Tok]]] = {
      prefixMatch(sequence) match {
        case None if sequence.isEmpty => Seq.empty
        case None => seqMatch(sequence.tail)
        case Some((m, rest)) => m +: seqMatch(rest)
      }
    }

    private[this] def makeInitialInstance(sequence: Seq[Tok]):AutomatonInstance[Tok] = {
      AutomatonInstance(StateInstance(this.initialState, this.initialState.transitions), List.empty, sequence)
    }

    @tailrec
    private[this] def collectPrefixMatches(iOpt: Option[AutomatonInstance[Tok]], matches:List[Seq[Match[Tok]]]):Seq[Seq[Match[Tok]]] = iOpt match {
      case Some(i) =>
        nextPrefixMatch(i) match {
          case Some(next) => collectPrefixMatches(backtrack(next), next.stateStack.map(_._2).reverse :: matches)
          case None =>
            matches.reverse
        }
      case None => matches.reverse
    }

    def allPrefixMatches(sequence: Seq[Tok]):Seq[Seq[Match[Tok]]] = {
      collectPrefixMatches(Some(makeInitialInstance(sequence)), List.empty)
    }

    /*
    Tries to match the automaton strictly at the beginning of the sequence.
     */
    private[this] def prefixMatch(sequence: Seq[Tok]): Option[(Seq[Match[Tok]], Seq[Tok])] = {
      nextPrefixMatch(makeInitialInstance(sequence))
        .map(i => (i.stateStack.map(_._2).reverse, i.seq))
    }
  }

}
