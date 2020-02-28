package fr.dcram.nlp4s.automata

import java.util.concurrent.atomic.AtomicInteger

object AutomatonFactory {
  import Quantifiers._

  private val idGen = new AtomicInteger(0)
  def sequence[Tok](transitionables:Transitionable[Tok]*):Automaton[Tok] = {
    val initState = transitionables.foldRight(newAcceptingState[Tok]){
      case (t, target) => State(idGen.incrementAndGet(), transitions = Seq(t.asTransition(target)))
    }
    Automaton(initState)
  }

  def asAutomaton[Tok](t:Transitionable[Tok]) = t match {
    case a:Automaton[Tok] => a
    case _ =>
      val target = newAcceptingState[Tok]
      Automaton(State(idGen.incrementAndGet(), transitions = Seq(t.asTransition(target))))
  }
  def or[Tok](transitionables:Transitionable[Tok]*):Transitionable[Tok] = {
    require(transitionables.size > 0)
    if(transitionables.size == 1)
      transitionables.head
    else {
      val target = newAcceptingState[Tok]
      Automaton(State(idGen.incrementAndGet(), transitions = transitionables.map(_.asTransition(target))))
    }
  }

  private def newAcceptingState[Tok]:State[Tok] = State[Tok](idGen.incrementAndGet(), List.empty, accepting = true)

  def named[Tok](name:String, a:Transitionable[Tok]*):Transitionable[Tok] = new Transitionable[Tok] {
    override def asTransition(target: State[Tok]): Transition[Tok] = AutTransit(Some(name), sequence(a:_*), target)
  }

  def quantified[Tok](t:Transitionable[Tok], quantifier:Quantifier):Transitionable[Tok] = quantifier match {
    case ZeroOne => zeroOne(t)
    case MN(m,n) if m == n => quantified(t, N(m))
    case MN(m,n) => sequence(quantified(t, N(m)),quantified(t, ZeroN(n-m)))
    case ZeroN(1) => zeroOne(t)
    case ZeroN(n) => sequence(Seq.fill(n)(quantified(t, ZeroOne)):_*)
    case NStar(0) => quantified(t, Star)
    case NStar(1) => quantified(t, Plus)
    case NStar(n) => sequence(quantified(t, N(n)), quantified(t, Star))
    case N(n) => sequence(Seq.fill(n)(t):_*)
    case Star => star(t)
    case Plus => sequence(t, quantified(t, Star))
    case e => throw new UnsupportedOperationException(s"Unsupported quantifier: ${e.getClass.getSimpleName}")
  }

  def zeroOne[Tok](t:Transitionable[Tok]):Transitionable[Tok] = {
    val target = newAcceptingState[Tok]
    Automaton(State(idGen.incrementAndGet(), transitions = Seq(t.asTransition(target), EpsilonTransit(target))))
  }
  def oneN[Tok](t:Transitionable[Tok]):Transitionable[Tok] = quantified(t, Plus)

  def star[Tok](t:Transitionable[Tok]):Transitionable[Tok] = {
    new AutomatonBuilder().initialState(1).state(2, accepting = true)
      .transition(1, 1, t)
      .epsilon(1, 2)
      .build
  }

}