package fr.dcram.nlp4s.automata

object AutomatonFactory {
  import Quantifiers._
  def sequence[Tok](transitionables:Transitionable[Tok]*):Automaton[Tok] = {
    val initState = transitionables.foldRight(newAcceptingState[Tok]){
      case (t, target) => State(transitions = Seq(t.asTransition(target)))
    }
    Automaton(initState)
  }

  def or[Tok](transitionables:Transitionable[Tok]*):Automaton[Tok] = {
    val target = newAcceptingState[Tok]
    Automaton(State(transitions = transitionables.map(_.asTransition(target))))
  }

  private def newAcceptingState[Tok]:State[Tok] = State[Tok](List.empty, accepting = true)

  def named[Tok](name:String, a:Automaton[Tok]):Automaton[Tok] = Automaton(State(transitions = Seq(AutTransit(Some(name), a, newAcceptingState))))

  def quantified[Tok](t:Transitionable[Tok], quantifier:Quantifier):Automaton[Tok] = quantifier match {
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

  def zeroOne[Tok](t:Transitionable[Tok]):Automaton[Tok] = {
    val target = newAcceptingState[Tok]
    Automaton(State(transitions = Seq(t.asTransition(target), EpsilonTransit(target))))
  }

  def star[Tok](t:Transitionable[Tok]):Automaton[Tok] = {
    new AutomatonBuilder().initialState(1).state(2, accepting = true)
      .transition(1, 1, t)
      .epsilon(1, 2)
      .build
  }

}