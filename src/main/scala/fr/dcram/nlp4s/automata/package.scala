package fr.dcram.nlp4s

package object automata {


  abstract class Transition[Tok](val target:State[Tok])
  case class EpsilonTransition[Tok](override val target:State[Tok]) extends Transition[Tok](target)
  case class AutomatonTransition[Tok](name:String, automaton:Automaton[Tok], override val target:State[Tok]) extends Transition[Tok](target)
  case class MatcherTransition[Tok](matcher:TokenMatcher[Tok], override val target:State[Tok]) extends Transition[Tok](target)

  trait TokenMatcher[Tok] {
    def matches(tok:Tok):Boolean
  }

  case class State[Tok](id:Int, transitions:Seq[Transition[Tok]], accepting:Boolean)

  case class Automaton[Tok](initialState:State[Tok]) {
    def seqMatch(sequence:Seq[Tok]):Seq[Seq[Match[Tok]]] = {
      prefixMatch(sequence) match {
        case None if sequence.isEmpty => Seq.empty
        case None => seqMatch(sequence.tail)
        case Some((m, rest)) => m +: seqMatch(rest)
      }
    }

    /*
    Tries to match the automaton strictly at the beginning of the sequence.
     */
    private[this] def prefixMatch(sequence:Seq[Tok]):Option[(Seq[Match[Tok]], Seq[Tok])] = {
      prefixRecursiveMatch(sequence, this.initialState, this.initialState.transitions, Seq.empty)
    }


    private[this] def prefixRecursiveMatch(sequence:Seq[Tok], current:State[Tok], transitions:Seq[Transition[Tok]], matches:Seq[Match[Tok]]): Option[(Seq[Match[Tok]], Seq[Tok])] = {
      (sequence, current.accepting, transitions) match {
        case (Nil, false, _) =>
          None
        case (Nil, true, _) =>
          Some((matches, Nil))
        case (_, false, Nil) =>
          None
        case (s, true, Nil) =>
          Some((matches, s))
        case (s, _, EpsilonTransition(target) +: restTransitions) =>
          prefixRecursiveMatch(s, target, target.transitions, matches).orElse(
            prefixRecursiveMatch(s, current, restTransitions, matches)
          )
        case (s@tok +: tail, _, MatcherTransition(matcher, target) +: restTransitions) if matcher.matches(tok) =>
          prefixRecursiveMatch(tail, target, target.transitions, matches :+ TokenMatch(tok) ).orElse(
            prefixRecursiveMatch(s, current, restTransitions, matches)
          )
        case (s, _, MatcherTransition(_, _) +: restTransitions) =>
            prefixRecursiveMatch(s, current, restTransitions, matches)
        case (s, _, head +: _) =>
          throw new UnsupportedOperationException(s"Not yet implemented: ${head.getClass.getSimpleName}")
      }

    }
  }
  abstract class Match[Tok]
  case class TokenMatch[Tok](token:Tok) extends Match[Tok]
  case class AutomatonMatch[Tok](name:String, matches:Seq[Match[Tok]]) extends Match[Tok]


/*
  trait PostfixAutomatonBuilder extends Automaton {
    type E <: Automaton
    type T = E
    def builder: AutomatonBuilder

    def |(a: Automaton):Automaton = builder.|(this, a)
    def ?():Automaton = builder.?(this)
    def +():Automaton = builder.+(this)
    def *():Automaton = builder.*(this)
    def ~(n:Int):Automaton = builder.~(this, n)
    def ~(m:Int, n:Int):Automaton = builder.~(this, m, n)

  }

  trait AutomatonBuilder {
    type T <: Automaton
    def |(a1: T, a2: T):T
    def ?(a:T):T
    def +(a:T):T
    def *(a:T):T
    def ~(a:T, n:Int):T
    def ~(a:T, m:Int, n:Int):T
  }
*/

}
