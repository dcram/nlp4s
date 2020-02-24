package fr.dcram.nlp4s

import fr.dcram.nlp4s.automata.Match

import scala.annotation.tailrec

package object automata {

  case class State[Tok](transitions:List[Transition[Tok]], accepting:Boolean = false)  {
    override def toString: String = s"State(${accepting})"}
  abstract class Transition[Tok](val target:State[Tok]) {
    def instantiate:TransitInst[Tok]
  }
  abstract class TransitInst[Tok](transit:Transition[Tok]) {
    def targetStateInstance:StateInst[Tok] = StateInst(transit.target)
    def _match(s:Seq[Tok]):Option[(Match[Tok], Seq[Tok])]
  }
  abstract class Match[Tok](val ti: TransitInst[Tok]) {
    def rollback(s:Seq[Tok]):Seq[Tok]
  }
  trait Transitionable[Tok] {
    def asTransition(target:State[Tok]):Transition[Tok]
  }
  trait TokenMatcher[Tok] extends Transitionable[Tok] {
    def matches(tok:Tok):Boolean
    override def asTransition(target: State[Tok]): Transition[Tok] = MatcherTransit(this, target)
  }


  // instance model
  case class EpsilonTransitInst[Tok](epsilonTransit:EpsilonTransit[Tok]) extends TransitInst[Tok](epsilonTransit) {
    override def _match(s: Seq[Tok]): Option[(Match[Tok], Seq[Tok])] = Some((EpsilonMatch(this), s))
  }
  case class MatcherTransitInst[Tok](matcherTransit: MatcherTransit[Tok]) extends TransitInst[Tok](matcherTransit) {
    override def _match(s: Seq[Tok]): Option[(Match[Tok], Seq[Tok])] = s match {
      case tok +: tail if(matcherTransit.matcher.matches(tok)) => Some((TokenMatch(tok, this), tail))
      case _ => None
    }
  }
  case class AutTransitInst[Tok](autTransit:AutTransit[Tok], autInst: AutInst[Tok]) extends TransitInst[Tok](autTransit) {
    override def _match(s: Seq[Tok]): Option[(Match[Tok], Seq[Tok])] = nextPrefixMatch(s, autInst).map{case (i,s) => (AutMatch(this), s)}
  }

  // transition model
  case class EpsilonTransit[Tok](override val target:State[Tok]) extends Transition[Tok](target) {
    override def toString: String = "Ɛ"
    override def instantiate: EpsilonTransitInst[Tok] = EpsilonTransitInst(this)
  }
  case class AutTransit[Tok](name:Option[String], automaton:Automaton[Tok], override val target:State[Tok]) extends Transition[Tok](target) {
    override def toString: String = s"${name.map(s => s"@($s)").getOrElse("")}Automaton"
    override def instantiate: AutTransitInst[Tok] = AutTransitInst(this, AutInst(automaton))
  }
  case class MatcherTransit[Tok](matcher:TokenMatcher[Tok], override val target:State[Tok]) extends Transition[Tok](target) {
    override def toString: String = matcher.getClass.getSimpleName
    override def instantiate: MatcherTransitInst[Tok] = MatcherTransitInst(this)
  }

  // match model
  case class EpsilonMatch[Tok](override val ti:EpsilonTransitInst[Tok]) extends Match(ti) {
    override def rollback(s: Seq[Tok]): Seq[Tok] = s
  }
  case class TokenMatch[Tok](token:Tok, override val ti:MatcherTransitInst[Tok]) extends Match(ti) {
    override def rollback(s: Seq[Tok]): Seq[Tok] = token +: s
  }
  case class AutMatch[Tok](override val ti:AutTransitInst[Tok]) extends Match(ti) {
    override def rollback(s: Seq[Tok]): Seq[Tok] = ti.autInst.rollback(s)
  }

  //instance model
  case class AutInst[Tok](current: StateInst[Tok], matchStack:List[(StateInst[Tok], Match[Tok])]) {
    def backtrack(s: Seq[Tok]): Option[(AutInst[Tok], Seq[Tok])] =  {
      def doBacktrack(s:Seq[Tok], ms:List[(StateInst[Tok], Match[Tok])]): Option[(AutInst[Tok], Seq[Tok])] = ms match {
        case (si, EpsilonMatch(_)) :: rest => Some((AutInst(si, rest), s))
        case (si, TokenMatch(tok, _)) :: rest => Some((AutInst(si, rest), tok +: s))
        case (si, AutMatch(ti)) :: rest => ti.autInst.backtrack(s).map{

          case (i, s) => AutInst(current, )
        }
          .orElse(Some((AutInst(si, rest), rollback(s))))
        case Nil => None
      }
      doBacktrack(s, matchStack)
    }

    def rollback(s: Seq[Tok]): Seq[Tok] =  {
      @tailrec
      def doRollback(s:Seq[Tok], ms:List[(StateInst[Tok], Match[Tok])]):Seq[Tok] = ms match {
        case (_, m) :: rest => doRollback(m.rollback(s), rest)
        case Nil => s
      }
      doRollback(s, matchStack)
    }
  }
  case class StateInst[Tok](accepting:Boolean, rest:List[TransitInst[Tok]])  {
    override def toString: String = s"StateInst(${accepting},${rest.map(_.toString).mkString("|")})"}

  object StateInst {
    def apply[Tok](state:State[Tok]) = new StateInst(state.accepting, state.transitions.map(_.instantiate))
    def apply[Tok](accepting:Boolean, rest:List[TransitInst[Tok]]) = new StateInst(accepting, rest)
  }

  object AutInst{
    def apply[Tok](automaton:Automaton[Tok]):AutInst[Tok] = new AutInst[Tok](StateInst[Tok](automaton.initialState),List.empty)
    def apply[Tok](current: StateInst[Tok], matchStack:List[(StateInst[Tok], Match[Tok])]):AutInst[Tok] = new AutInst(current,matchStack)
  }

  private[this] def backtrack[Tok](s: Seq[Tok], i:AutInst[Tok]):Option[(Seq[Tok], AutInst[Tok])] = (s, i) match {
    case (_, AutInst(_, Nil)) => None
    case (s, AutInst(_, (si, EpsilonMatch(ti)) :: matchStack)) => Some(s, AutInst(si, matchStack))
    case (s, AutInst(_, (si, TokenMatch(tok, ti)) :: matchStack)) => Some(tok +: s, AutInst(si, matchStack))
    case (s, AutInst(_, (si, AutMatch(ti)) :: matchStack)) => backtrack(???, ti.autInst) match {
      case Some((s, i)) => Some(???, AutInst(si, matchStack))
      case None => Some(ti. +: s, AutInst(si, matchStack))
    }
  }

  /*
  The reactor method of automaton instance matching
   */
  @tailrec
  private[this] def nextPrefixMatch[Tok](s:Seq[Tok], i:AutInst[Tok]):Option[(AutInst[Tok], Seq[Tok])] = {
    (s, i) match {
      case (Nil, i) if i.current.accepting =>
        Some((i, s))
      case (Nil, i)  =>
        backtrack(s, i) match {
          case Some((s, bi)) => nextPrefixMatch[Tok](s, bi)
          case None => None
        }
      case (_, AutInst(StateInst(accepting, Nil), _)) if accepting =>
        Some((i, s))
      case (s, AutInst(StateInst(_, Nil), _)) =>
        backtrack(s, i) match {
          case Some((s, bi)) => nextPrefixMatch[Tok](s,bi)
          case None => None
        }
      case (s, AutInst(StateInst(accepting, t +: rTransit), matchStack)) =>
        val nextCur = StateInst(accepting, rTransit)
        t._match(s) match {
          case Some((m, rSeq)) => nextPrefixMatch(rSeq, AutInst(t.targetStateInstance, matchStack :+ (nextCur, m)))
          case None => nextPrefixMatch(s, AutInst(nextCur, matchStack))
        }
    }
  }


  object Automaton {
    def empty[Tok]:Automaton[Tok] = Automaton[Tok](State(transitions = List.empty, accepting = true))
  }

  case class Automaton[Tok](initialState:State[Tok]) extends Transitionable[Tok] {
    override def asTransition(target: State[Tok]): Transition[Tok] = new AutTransit[Tok](None, this, target)
    def seqMatch(sequence: Seq[Tok]): Seq[Seq[Match[Tok]]] = {
      prefixMatch(sequence) match {
        case None if sequence.isEmpty => Seq.empty
        case None => seqMatch(sequence.tail)
        case Some((m, rest)) => m +: seqMatch(rest)
      }
    }

    @tailrec
    private[this] def collectPrefixMatches(s:Seq[Tok], iOpt: Option[AutInst[Tok]], matches:List[AutMatch[Tok]]):Seq[AutMatch[Tok]] = iOpt match {
      case Some(i) =>
        nextPrefixMatch(s, i) match {
          case Some((m, _)) => collectPrefixMatches(s, backtrack(s, m.ti).map(_._2), matches :+ m)
          case None =>
            matches.reverse
        }
      case None => matches.reverse
    }

    def allPrefixMatches(s: Seq[Tok]):Seq[AutMatch[Tok]] = {
      collectPrefixMatches(s, Some(AutInst(this)), List.empty)
    }

    /*
    Tries to match the automaton strictly at the beginning of the sequence.
     */
    private[this] def prefixMatch(sequence: Seq[Tok]): Option[(AutMatch[Tok], Seq[Tok])] = {
      nextPrefixMatch(sequence, AutInst(this))
    }
  }

}
