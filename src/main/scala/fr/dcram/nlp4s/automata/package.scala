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

  case class State[Tok](id:Long,transitions: Seq[Transition[Tok]], accepting:Boolean = false)  {
    override def toString: String = s"State($accepting)"}


  trait Instantiable[Tok] {
    def instantiate:TransitInst[Tok]
  }
  trait Transition[Tok] extends Instantiable[Tok]

  trait ExtrinsicTransition[Tok] extends Transition[Tok] {
    def _match(s:Seq[Token[Tok]]):Option[(State[Tok], Seq[Token[Tok]], Seq[Token[Tok]])]
    def instantiate:TransitInst[Tok] = new TransitInst[Tok](this) {
      override def _match(s: Seq[Token[Tok]]): Option[(StateInst[Tok], Match[Tok], Seq[Token[Tok]])] = ExtrinsicTransition.this._match(s)
        .map {
          case (state, matchedTokens,rest) => (StateInst(state), TokenMatch(matchedTokens, this), rest)
        }
    }
  }

  abstract class TransitInst[Tok](val transit:Transition[Tok]) {
    def _match(s:Seq[Token[Tok]]):Option[(StateInst[Tok], Match[Tok], Seq[Token[Tok]])]
  }

  abstract class Match[Tok](val ti:TransitInst[Tok]) {
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
    override def _matches(tok:Token[Tok]): Boolean = tok match {
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
  case class AutTransitInst[Tok](autTransit:AutTransit[Tok], autInst: AutInst[Tok]) extends TransitInst[Tok](autTransit) {
    override def toString: String = "Aut"
    override def _match(s: Seq[Token[Tok]]): Option[(StateInst[Tok], Match[Tok], Seq[Token[Tok]])] = nextPrefixMatch(s, autInst).map{case (restS, matchedI) => (StateInst(autTransit.target), AutMatch(this.copy(autInst = matchedI)), restS)}
  }

  // transition model
  case class EpsilonTransit[Tok](target:State[Tok]) extends ExtrinsicTransition[Tok] {
    override def toString: String = "Ɛ"
    override def _match(s: Seq[Token[Tok]]): Option[(State[Tok], Seq[Token[Tok]], Seq[Token[Tok]])] = s match {
      case SeqStart +: _ =>
        None // does not matches sequence start
      case s =>
        Some((target, Seq.empty, s))

    }
  }

  case class MatcherTransit[Tok](matcher:TokenMatcher[Tok], target:State[Tok]) extends ExtrinsicTransition[Tok] {
    override def toString: String = matcher.getClass.getSimpleName

    override def _match(s: Seq[Token[Tok]]): Option[(State[Tok], Seq[Token[Tok]], Seq[Token[Tok]])] = s match {
      case tok +: tail if matcher._matches(tok) => Some((target, Seq(tok), tail))
      case _ => None
    }
  }

  case class AutTransit[Tok](name:Option[String], automaton:Automaton[Tok], target:State[Tok]) extends Transition[Tok] with Instantiable[Tok] {
    override def toString: String = s"${name.map(s => s"@($s)").getOrElse("")}Automaton"
    override def instantiate: AutTransitInst[Tok] = AutTransitInst(this, AutInst.fromAut(automaton))
  }


  // match model
  case class TokenMatch[Tok](tokens:Seq[Token[Tok]], override val ti:TransitInst[Tok]) extends Match(ti) {
    override def toString: String = tokens.toString
    override def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] = tokens ++ s
  }
  case class AutMatch[Tok](override val ti:AutTransitInst[Tok]) extends Match(ti) {
    override def toString: String = ti.autInst.toString
    override def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] = ti.autInst.rollback(s)
  }

  case class StateInst[Tok](state:State[Tok],accepting:Boolean, rest: List[TransitInst[Tok]])  {
    override def toString: String = s"SI(${state.id}${if (state.accepting) "✓" else ""},${rest.map(_.toString).mkString("|")})"
    def reset:StateInst[Tok] = StateInst(state)
  }


  object StateInst {
    def apply[Tok](state:State[Tok]):StateInst[Tok] = new StateInst(state, state.accepting, state.transitions.toStream.map(_.instantiate).toList)
    def apply[Tok](accepting:Boolean, state:State[Tok], rest:List[TransitInst[Tok]]):StateInst[Tok] = new StateInst( state, accepting,rest)
  }

  object AutInst{
    def fromAut[Tok](automaton:Automaton[Tok]):AutInst[Tok] = new AutInst[Tok](StateInst[Tok](automaton.initialState),List.empty)
  }


  //instance model
  case class AutInst[Tok](current: StateInst[Tok], matchStack:List[(StateInst[Tok], Match[Tok])]) {

    def tokLength:Int = {
      @tailrec
      def doTokLength(l:Int, ms:List[(StateInst[Tok], Match[Tok])]):Int = {
        ms match {
          case Nil => l
          case (_, TokenMatch(tokens, _)) :: tl => doTokLength(tokens.length+l, tl)
          case (_, AutMatch(ti)) :: tl => doTokLength(ti.autInst.tokLength+l, tl)
          case m :: _ => throw new IllegalStateException(s"Unexpected match type: $m")
        }
      }
      doTokLength(0, matchStack)
    }

    private def tokenToString[Tok](stack:List[(StateInst[Tok], Match[Tok])]):String = stack match {
      case Nil => ""
      case (_, m) :: tail =>
        val transitionlabel = m.ti match {
          case ati:AutTransitInst[Tok] => s"[${ati.autInst.tokenToString(ati.autInst.matchStack)}]"
          case o => o.toString
        }
        s"${tokenToString(tail)}$transitionlabel"
    }
    override def toString: String = {
      s"@${current.state.id}[${tokenToString(matchStack)}]${if (current.accepting) "✓" else "…"}"
    }

    def rollback(s: Seq[Token[Tok]]): Seq[Token[Tok]] =  {
      @tailrec
      def doRollback(s:Seq[Token[Tok]], ms:List[(StateInst[Tok], Match[Tok])]):Seq[Token[Tok]] = ms match {
        case (_, m) :: rest => doRollback(m.rollback(s), rest)
        case Nil => s
      }
      doRollback(s, matchStack)
    }


    def backtrack(s: Seq[Token[Tok]]): Option[(Seq[Token[Tok]], AutInst[Tok])] =  {
      val res = matchStack match {
        case (srcSi, AutMatch(ti)) :: mRest =>

          ti.autInst.backtrack(s) match {
          case Some((bs, bi)) =>
            nextPrefixMatch(bs, bi) match {
                case Some((s2, i2)) =>
                  val bi2 = AutInst(
                    current.reset,
                    (srcSi, AutMatch(ti.copy(autInst = i2))) :: mRest)
                  Some((s2, bi2))
                case None =>
                  val s2 = bi.rollback(bs)
                  val i2 = AutInst(srcSi, mRest)
                  Some((s2, i2))
              }
            case None =>
              Some((ti.autInst.rollback(s), AutInst(srcSi, mRest)))
          }
        case (srcSi, m) :: mRest =>
          Some((m.rollback(s), AutInst(srcSi, mRest)))
        case Nil =>
          None
      }
      res
    }

  }

  /*
  The reactor method of automaton instance matching
   */
  @tailrec
  private[this] def nextPrefixMatch[Tok](s:Seq[Token[Tok]], i:AutInst[Tok]):Option[(Seq[Token[Tok]], AutInst[Tok])] = {
//    println(s"${"  "*i.matchStack.size}${i}    ${i.current}")
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
      case (_, AutInst(StateInst(state, accepting, Nil), _)) if accepting =>
        Some((s, i))
      case (s, AutInst(StateInst(state, _, Nil), _)) =>
        i.backtrack(s) match {
          case Some((bs, bi)) =>
            nextPrefixMatch[Tok](bs,bi)
          case None =>
            None
        }
      case (s, AutInst(StateInst(state, accepting, t :: rTransit), matchStack)) =>
        val backtrackSrcSi = StateInst(state, accepting, rTransit)
        t._match(s) match {
          case Some((target, m, rSeq)) =>
            nextPrefixMatch(rSeq, AutInst(target, (backtrackSrcSi, m) :: matchStack))
          case None =>
            nextPrefixMatch(s, AutInst(backtrackSrcSi, matchStack))
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
      AutInst.fromAut(a),
      List.empty)
  }

  /*
  Tries to match the automaton strictly at the beginning of the sequence.
   */
  private[this] def prefixSeqMatch[Tok](a:Automaton[Tok], sequence: Seq[Token[Tok]]): Option[(Seq[Token[Tok]], AutInst[Tok])] = {
    nextPrefixMatch(sequence, AutInst.fromAut(a))
  }


}
