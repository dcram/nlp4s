package fr.dcram.nlp4s

import scala.annotation.tailrec

package object automata {
  abstract class Token[+Tok]
  object SeqStart extends Token[Nothing]
  object SeqEnd extends Token[Nothing]
  object ^ extends TokenMatcher[Nothing] {
    override def _matches(tok: Token[Nothing]): Boolean = tok == SeqStart
  }
  object $ extends TokenMatcher[Nothing] {
    override def _matches(tok: Token[Nothing]): Boolean = tok == SeqEnd
  }
  case class UserToken[T](t:T) extends Token[T]

  case class Automaton[Tok](initialState:State[Tok]) extends Transitionable[Tok] {
    override def asTransition(target: State[Tok]): Transition[Tok] = new AutTransit[Tok](None, this, target)
    def asNamedTransition(name: String, target: State[Tok]): Transition[Tok] = new AutTransit[Tok](Some(name), this, target)
  }
  object Automaton {
    def empty[Tok]:Automaton[Tok] = Automaton[Tok](State(transitions = List.empty, accepting = true))
  }
  case class State[Tok](transitions:Seq[Transition[Tok]], accepting:Boolean = false)  {
    override def toString: String = s"State($accepting)"}
  abstract class Transition[Tok](val target:State[Tok]) {
    def instantiate:TransitInst[Tok]
  }
  abstract class TransitInst[Tok](transit:Transition[Tok]) {
    def targetStateInstance:StateInst[Tok] = StateInst(transit.target)
    def _match(s:Seq[Token[Tok]]):Option[(Match[Tok], Seq[Token[Tok]])]
  }
  abstract class Match[Tok](val ti: TransitInst[Tok]) {
    def rollback(s:Seq[Token[Tok]]):Seq[Token[Tok]]
  }
  trait Transitionable[Tok] {
    def asTransition(target:State[Tok]):Transition[Tok]
  }
  trait TokenMatcher[Tok] extends Transitionable[Tok] {
    def _matches(tok:Token[Tok]):Boolean
    override def asTransition(target: State[Tok]): Transition[Tok] = MatcherTransit(this, target)
  }
  trait UserTokenMatcher[Tok] extends TokenMatcher[Tok] {
    def matches(tok:Tok):Boolean
    override def _matches(tok: Token[Tok]): Boolean = tok match {
      case SeqStart =>
        false
      case SeqEnd =>
        false
      case UserToken(tok) =>
        matches(tok)
      case e => throw  new UnsupportedOperationException(s"Unexpected matcher $e")
    }
  }


  // instance model
  case class EpsilonTransitInst[Tok](epsilonTransit:EpsilonTransit[Tok]) extends TransitInst[Tok](epsilonTransit) {
    override def _match(s: Seq[Token[Tok]]): Option[(Match[Tok], Seq[Token[Tok]])] = s match {
      case SeqStart +: _ =>
        None // does not matches sequence start
      case SeqEnd +: _ =>
        Some((EpsilonMatch(this), s))
      case s =>
        Some((EpsilonMatch(this), s))
    }
  }
  case class MatcherTransitInst[Tok](matcherTransit: MatcherTransit[Tok]) extends TransitInst[Tok](matcherTransit) {
    override def _match(s: Seq[Token[Tok]]): Option[(Match[Tok], Seq[Token[Tok]])] = s match {
      case tok +: tail if matcherTransit.matcher._matches(tok) => Some((TokenMatch(tok, this), tail))
      case _ => None
    }
  }
  case class AutTransitInst[Tok](autTransit:AutTransit[Tok], autInst: AutInst[Tok]) extends TransitInst[Tok](autTransit) {
    override def _match(s: Seq[Token[Tok]]): Option[(Match[Tok], Seq[Token[Tok]])] = nextPrefixMatch(s, autInst).map{case (restS, matchedI) => (AutMatch(this.copy(autInst = matchedI)), restS)}
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
    override def toString: String = "Ɛ"
    override def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] = s
  }
  case class TokenMatch[Tok](token:Token[Tok], override val ti:MatcherTransitInst[Tok]) extends Match(ti) {
    override def toString: String = token.toString
    override def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] = token +: s
  }
  case class AutMatch[Tok](override val ti:AutTransitInst[Tok]) extends Match(ti) {
    override def toString: String = ti.autInst.toString
    override def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] = ti.autInst.rollback(s)
  }

  //instance model
  case class AutInst[Tok](current: StateInst[Tok], matchStack:List[(StateInst[Tok], Match[Tok])]) {

    def tokLength:Int = {
      @tailrec
      def doTokLength(l:Int, ms:List[(StateInst[Tok], Match[Tok])]):Int = {
        ms match {
          case Nil => l
          case (_, EpsilonMatch(_)) :: tl => doTokLength(l, tl)
          case (_, TokenMatch(_, _)) :: tl => doTokLength(1+l, tl)
          case (_, AutMatch(ti)) :: tl => doTokLength(ti.autInst.tokLength+l, tl)
          case m :: _ => throw new IllegalStateException(s"Unexpected match type: $m")
        }
      }
      doTokLength(0, matchStack)
    }

    override def toString: String = {
      def toString(stack:List[(StateInst[Tok], Match[Tok])]):String = stack match {
        case Nil => ""
        case (_, m) :: tail => s"${toString(tail)}$m"
      }
      s"[${toString(matchStack)}]${if (current.accepting) "✓" else "…"}"
    }

    def backtrack(s: Seq[Token[Tok]]): Option[(Seq[Token[Tok]], AutInst[Tok])] =  {
      matchStack match {
        case (si, EpsilonMatch(_)) :: rest =>
          Some((s, AutInst(si, rest)))
        case (si, TokenMatch(tok, _)) :: rest =>
          Some((tok +: s, AutInst(si, rest)))
        case (si, AutMatch(ti)) :: mRest =>
          ti.autInst.backtrack(s) match {
          case Some((bs, bi)) =>
            nextPrefixMatch(bs, bi) match {
                case Some((s2, i2)) =>
                  val bi2 = AutInst(current, (si, AutMatch(ti.copy(autInst = i2))) :: mRest)
                  Some((s2, bi2))
                case None =>
                  val s2 = bi.rollback(bs)
                  val i2 = AutInst(si, mRest)
                  Some((s2, i2))
              }
            case None =>
              Some((ti.autInst.rollback(s), AutInst(si, mRest)))
          }
        case Nil =>
          None
        case h :: _ => throw new IllegalStateException(s"Unexpected element: $h")
      }
    }

    def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] =  {
      @tailrec
      def doRollback(s:Seq[Token[Tok]], ms:List[(StateInst[Tok], Match[Tok])]):Seq[Token[Tok]] = ms match {
        case (_, m) :: rest => doRollback(m.rollback(s), rest)
        case Nil => s
      }
      doRollback(s, matchStack)
    }
  }
  case class StateInst[Tok](accepting:Boolean, rest:List[TransitInst[Tok]])  {
    override def toString: String = s"StateInst($accepting,${rest.map(_.toString).mkString("|")})"}

  object StateInst {
    def apply[Tok](state:State[Tok]) = new StateInst(state.accepting, state.transitions.map(_.instantiate).toList)
    def apply[Tok](accepting:Boolean, rest:List[TransitInst[Tok]]) = new StateInst(accepting, rest)
  }

  object AutInst{
    def apply[Tok](automaton:Automaton[Tok]):AutInst[Tok] = new AutInst[Tok](StateInst[Tok](automaton.initialState),List.empty)
    def apply[Tok](current: StateInst[Tok], matchStack:List[(StateInst[Tok], Match[Tok])]):AutInst[Tok] = new AutInst(current,matchStack)
  }

  /*
  The reactor method of automaton instance matching
   */
  @tailrec
  private[this] def nextPrefixMatch[Tok](s:Seq[Token[Tok]], i:AutInst[Tok]):Option[(Seq[Token[Tok]], AutInst[Tok])] = {
    (s, i) match {
      case (Nil, i) if i.current.accepting =>
        Some((s,i))
      case (Nil, i)  =>
        i.backtrack(s) match {
          case Some((s, bi)) =>
            nextPrefixMatch[Tok](s, bi)
          case None =>
            None
        }
      case (_, AutInst(StateInst(accepting, Nil), _)) if accepting =>
        Some((s, i))
      case (s, AutInst(StateInst(_, Nil), _)) =>
        i.backtrack(s) match {
          case Some((bs, bi)) =>
            nextPrefixMatch[Tok](bs,bi)
          case None =>
            None
        }
      case (s, AutInst(StateInst(accepting, t :: rTransit), matchStack)) =>
        val nextCur = StateInst(accepting, rTransit)
        t._match(s) match {
          case Some((m, rSeq)) =>
            nextPrefixMatch(rSeq, AutInst(t.targetStateInstance, (nextCur, m) :: matchStack))
          case None =>
            nextPrefixMatch(s, AutInst(nextCur, matchStack))
        }
    }
  }



  def seqMatch[Tok](a:Automaton[Tok], sequence: Seq[Tok]): Seq[RegexMatch[Tok]] = {
    @tailrec
    def doSeqMatch(a:Automaton[Tok], sequence: Seq[Token[Tok]], matches:List[RegexMatch[Tok]]): List[RegexMatch[Tok]] = {
      prefixSeqMatch(a, sequence) match {
        case None if sequence.isEmpty =>
          matches
        case None =>
          doSeqMatch(a, sequence.tail, matches)
        case Some((restSeq, i)) =>
          doSeqMatch(a, if(i.tokLength == 0 && restSeq.nonEmpty) restSeq.tail else restSeq, RegexMatch(i) :: matches)
      }
    }

    doSeqMatch(a, tokenify(sequence), List.empty).reverse
  }

  private def tokenify[Tok](sequence: Seq[Tok]):Seq[Token[Tok]] = {
    SeqStart +: sequence.map(UserToken.apply) :+ SeqEnd
  }

  private[this] def collectPrefixMatches[Tok](s:Seq[Token[Tok]], i: AutInst[Tok], matches:List[RegexMatch[Tok]]):Seq[RegexMatch[Tok]] = {
    nextPrefixMatch(s, i) match {
      case Some((s, i)) =>
        val maybeTuple = i.backtrack(s)
        maybeTuple.map{
          case (bs,bi) =>
            collectPrefixMatches(bs, bi,  RegexMatch(i) :: matches)
        }.getOrElse(matches.reverse)
      case None =>
        matches.reverse
    }
  }

  def allPrefixMatches[Tok](a:Automaton[Tok], s: Seq[Tok]):Seq[RegexMatch[Tok]] = {
    collectPrefixMatches(
      tokenify(s).tail, // rm the SeqStart token
      AutInst(a),
      List.empty)
  }

  /*
  Tries to match the automaton strictly at the beginning of the sequence.
   */
  private[this] def prefixSeqMatch[Tok](a:Automaton[Tok], sequence: Seq[Token[Tok]]): Option[(Seq[Token[Tok]], AutInst[Tok])] = {
    nextPrefixMatch(sequence, AutInst(a))
  }


  case class RegexMatch[Tok](tokens:Seq[Token[Tok]], groups:Map[String, Seq[Seq[Token[Tok]]]])

  object RegexMatch {
    def apply[Tok](i:AutInst[Tok]):RegexMatch[Tok] = {
      def merge[K, V](m1:Map[K, Seq[V]], m2:Map[K, Seq[V]]):Map[K, Seq[V]] =
        (m1.keySet ++ m2.keySet).map { i => i -> (m1.getOrElse(i, Seq.empty) ++ m2.getOrElse(i, Seq.empty)) }.toMap

      def collect(matchStack:List[(StateInst[Tok], Match[Tok])], tokens:List[Token[Tok]], groups:Map[String, Seq[Seq[Token[Tok]]]]):(List[Token[Tok]], Map[String, Seq[Seq[Token[Tok]]]]) = {
        matchStack match {
          case (_, EpsilonMatch(_)) :: rest =>
            collect(rest, tokens, groups)
          case (_, TokenMatch(tok, _)) :: rest =>
            collect(rest, tok :: tokens, groups)
          case (_, AutMatch(ti)) :: rest =>
            val (subTokens, subGroups) = collect(ti.autInst.matchStack, List.empty, Map.empty)
            ti.autTransit.name match {
              case Some(name) =>
                collect(rest, subTokens ::: tokens, merge(merge(groups, subGroups), Map(name -> Seq(subTokens))))
              case None =>
                collect(rest, subTokens ::: tokens , merge(groups, subGroups))
            }
          case Nil =>
            (tokens, groups)
          case h :: _ =>
            throw new IllegalStateException(s"Unexpected element: $h")
        }
      }
      val (tokens, groups) = collect(i.matchStack, List.empty, Map.empty)
      new RegexMatch(tokens, groups)
    }
  }
}
