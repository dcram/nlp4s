package fr.dcram.nlp4s.automata

object AutomatonFactory {
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

  def quantified[Tok](t:Transitionable[Tok], quantifier:Quantifiers.Quantifier):Automaton[Tok] = quantifier match {
    case Quantifiers.ZeroOne => zeroOne(t)
    case Quantifiers.MN(m,n) if m == n => quantified(t, Quantifiers.N(m))
    case Quantifiers.MN(m,n) => sequence(quantified(t, Quantifiers.N(m)),quantified(t, Quantifiers.ZeroN(m-n)))
    case Quantifiers.ZeroN(n) => sequence(Seq.fill(n)(quantified(t, Quantifiers.ZeroOne)):_*)
    case Quantifiers.NStar(n) => sequence(quantified(t, Quantifiers.N(n)), quantified(t, Quantifiers.Star))
    case Quantifiers.N(n) => sequence(Seq.fill(n)(t):_*)
    case Quantifiers.Star => star(t)
    case Quantifiers.Plus => sequence(t, quantified(t, Quantifiers.Star))
    case e => throw new UnsupportedOperationException(s"Unsupported quantifier: ${e.getClass.getSimpleName}")
  }

  def zeroOne[Tok](t:Transitionable[Tok]):Automaton[Tok] = {
    val target = newAcceptingState[Tok]
    Automaton(State(transitions = Seq(t.asTransition(target), EpsilonTransit(target))))
  }

  def star[Tok](t:Transitionable[Tok]):Automaton[Tok] = {
    new AutomatonBuilder().initialState(1).state(2, accepting = true)
      .epsilon(1, 2)
      .transition(1, 2, t)
      .build
  }

}