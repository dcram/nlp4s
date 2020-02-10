package fr.dcram.nlp4s

import scala.collection.mutable

package object automata {


  class AutomatonBuilder[Tok <: Token]() {

    private[this] var initialState:Option[State] = None
    private[this] val states = new mutable.HashMap[Int, State]()
    private[this] def state(id:Int, accepting:Boolean=false):State = {
      if(!states.contains(id))
        states.put(id, State(id, new mutable.ListBuffer[Transition](), accepting))
      states(id)
    }
    def initial(id:Int):AutomatonBuilder[Tok] = {
      require(initialState.isEmpty, "Initial state already defined")
      initialState = Some(state(id))
      this
    }
    def addState(id:Int, initial:Boolean = false, accepting:Boolean = false):AutomatonBuilder[Tok] = {
      require(!states.contains(id), s"State $id already defined")
      require(!initial && initialState.isDefined, "Already an initial state")
      state(id, accepting)
      this
    }
    def transition(fromState: Int, toState:Int, matcher:TokenMatcher[Tok], toStateAccepting:Boolean = false):AutomatonBuilder[Tok] = {
      val transition = new MatcherTransition[Tok](matcher, state(toState, toStateAccepting))
      state(fromState).transitions.asInstanceOf[mutable.ListBuffer[Transition]].append(transition)
      this
    }
    def build:Automaton  = {
      require(initialState.isDefined, "No initial state defined")
      AnonAutomaton(initialState.get)
    }
  }

  class Token
  object EndToken extends Token

  abstract class Transition(val target:State)
  case class EpsilonTransition(override val target:State) extends Transition(target)
  case class AutomatonTransition(automaton:Automaton, override val target:State) extends Transition(target)
  case class MatcherTransition[Tok <: Token](matcher:TokenMatcher[Tok], override val target:State) extends Transition(target)

  trait TokenMatcher[Tok <: Token] {
    def matches(tok:Tok):Boolean
  }

  case class State(id:Int, transitions:Seq[Transition], accepting:Boolean)

  case class Match(tokens:Iterable[Token])
  abstract class Automaton(initialState:State)
  case class AnonAutomaton(initialState:State) extends Automaton(initialState)
  case class NamedAutomaton(name:String,initialState:State) extends Automaton(initialState)

  def seqMatch[Tok <: Token](sequence: Seq[Tok], automaton:Automaton):Option[(Match, Seq[Tok])] = {

  }

  def matchesIn[Tok <: Token](current:State, sequence: Seq[Tok], matchedTokens:List[Tok]):Option[(Match, Seq[Tok])] = {
    (sequence, current) match {
      case (Nil, State(_, false)) => None
      case (Nil, State(_, true)) => Some(Match(tokens = matchedTokens))
      case (tok :: tail, State(transitions, accepting)) =>
        transitions match {
          case EpsilonTransition(target) :: _ => matchesIn(target, sequence, matchedTokens)
          case AutomatonTransition(a, target) =>
        }
    }
  }

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
