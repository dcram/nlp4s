package fr.dcram.nlp4s.automata

import scala.collection.mutable

class AutomatonBuilder[Tok]() {

  private[this] var initialState:Option[State[Tok]] = None
  private[this] val states = new mutable.HashMap[Int, State[Tok]]()
  private[this] def state(id:Int, accepting:Boolean=false):State[Tok] = {
    if(!states.contains(id))
      states.put(id, State[Tok](new mutable.ArrayBuffer[Transition[Tok]](), accepting))
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
    state(fromState).transitions.asInstanceOf[mutable.ArrayBuffer[Transition[Tok]]].append(transition)
    this
  }
  def build:Automaton[Tok]  = {
    require(initialState.isDefined, "No initial state defined")
    Automaton(initialState.get)
  }
}
