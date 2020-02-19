package fr.dcram.nlp4s

import scala.annotation.tailrec

package object automata {

  trait Transitionable[Tok] {
    def asTransition(target:State[Tok]):Transition[Tok]
  }
  trait TokenMatcher[Tok] extends Transitionable[Tok] {
    def matches(tok:Tok):Boolean

    override def asTransition(target: State[Tok]): Transition[Tok] = new MatcherTransition[Tok](this, target)
  }

  abstract class Transition[Tok](val target:State[Tok])
  case class EpsilonTransition[Tok](override val target:State[Tok]) extends Transition[Tok](target)
  case class AutomatonTransition[Tok](name:Option[String], automaton:Automaton[Tok], override val target:State[Tok]) extends Transition[Tok](target)
  case class MatcherTransition[Tok](matcher:TokenMatcher[Tok], override val target:State[Tok]) extends Transition[Tok](target)
  case class State[Tok](transitions:Seq[Transition[Tok]], accepting:Boolean = false)
  case class StateInstance[Tok](state:State[Tok], rest:Seq[Transition[Tok]])
  case class AutomatonInstance[Tok](current: StateInstance[Tok], stack:List[(StateInstance[Tok], Match[Tok])], seq:Seq[Tok])
  abstract class Match[Tok]
  case class EpsilonMatch[Tok](transition:EpsilonTransition[Tok]) extends Match[Tok]
  case class TokenMatch[Tok](token:Tok, transition:MatcherTransition[Tok]) extends Match[Tok]
  case class AutomatonMatch[Tok](name:String, matches:List[Match[Tok]]) extends Match[Tok]

  private[this] def backtrack[Tok](i:AutomatonInstance[Tok]):Option[AutomatonInstance[Tok]] = i match {
    case AutomatonInstance(_, Nil, _) => None
    case AutomatonInstance(_, (si, EpsilonMatch(_)) :: stack, seq) => Some(AutomatonInstance(si, stack, seq))
    case AutomatonInstance(_, (si, TokenMatch(tok, _)) :: stack, seq) => Some(AutomatonInstance(si, stack, tok +: seq))
  }

  /*
  The reactor method of automaton instance matching
   */
  @tailrec
  private[this] def nextPrefixMatch[Tok](i:AutomatonInstance[Tok]):Option[AutomatonInstance[Tok]] = {
    i match {
      case AutomatonInstance(StateInstance(currentState, _), _, Nil) if currentState.accepting =>
        Some(i)
      case AutomatonInstance(StateInstance(_, _), _, Nil)  =>
        backtrack(i) match {
          case Some(bi) => nextPrefixMatch[Tok](bi)
          case None => None
        }
      case AutomatonInstance(StateInstance(currentState, Nil), _, _) if currentState.accepting =>
        Some(i)
      case AutomatonInstance(StateInstance(_, Nil), _, _) =>
        backtrack(i) match {
          case Some(bi) => nextPrefixMatch[Tok](bi)
          case None => None
        }
      case AutomatonInstance(StateInstance(currentState, EpsilonTransition(target) +: restTransitions), stack, s) =>
        nextPrefixMatch(AutomatonInstance(
          StateInstance(target, target.transitions),
          (StateInstance(currentState, restTransitions), EpsilonMatch(EpsilonTransition(target))) :: stack,
          s
        ))
      case AutomatonInstance(StateInstance(currentState, MatcherTransition(matcher, target) +: restTransitions), stack, tok +: restSeq) if matcher.matches(tok) =>
        nextPrefixMatch(AutomatonInstance(
          StateInstance(target, target.transitions),
          (StateInstance(currentState, restTransitions), TokenMatch(tok, MatcherTransition(matcher, target))) :: stack,
          restSeq
        ))
      case AutomatonInstance(StateInstance(currentState, _ +: restTransitions), stack, seq) =>
        nextPrefixMatch(AutomatonInstance(
          StateInstance(currentState, restTransitions),
          stack,
          seq
        ))
      case AutomatonInstance(StateInstance(_, head :: _), _, _) =>
        throw new UnsupportedOperationException(s"Not yet implemented: ${head.getClass.getSimpleName}")
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
          case Some(next) => collectPrefixMatches(backtrack(next), next.stack.map(_._2).reverse :: matches)
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
        .map(i => (i.stack.map(_._2).reverse, i.seq))
    }
  }

}
