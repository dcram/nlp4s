package fr.dcram.nlp4s.automata

import scala.collection.mutable

class AutomatonBuilder[Tok]() {

  private[this] var initialState:Option[State[Tok]] = None
  private[this] val states = new mutable.HashMap[Int, State[Tok]]()
  private[this] def getState(id:Int):State[Tok] = states(id)
  def initialState(id:Int):AutomatonBuilder[Tok] = {
    require(initialState.isEmpty, "Initial state already defined")
    state(id)
    initialState = Some(getState(id))
    this
  }
  def state(id:Int, accepting:Boolean = false):AutomatonBuilder[Tok] = {
    require(!states.contains(id), s"State $id already defined")
    states.put(id, State[Tok](id, new mutable.ListBuffer[Transition[Tok]](), accepting))
    this
  }
  def matcherTransition(fromState: Int, toState:Int, matcher:TokenMatcher[Tok]):AutomatonBuilder[Tok] = {
    val transition = new MatcherTransit[Tok](matcher, getState(toState))
    getState(fromState).transitions.asInstanceOf[mutable.ListBuffer[Transition[Tok]]].append(transition)
    this
  }
  def transition(fromState: Int, toState:Int, t:Transitionable[Tok]):AutomatonBuilder[Tok] = {
    getState(fromState).transitions.asInstanceOf[mutable.ListBuffer[Transition[Tok]]].append(t.asTransition(getState(toState)))
    this
  }
  def namedTransition(name:String, fromState: Int, toState:Int, t:Automaton[Tok]):AutomatonBuilder[Tok] = {
    getState(fromState).transitions.asInstanceOf[mutable.ListBuffer[Transition[Tok]]].append(t.asNamedTransition(name, getState(toState)))
    this
  }
  def epsilon(fromState: Int, toState:Int):AutomatonBuilder[Tok] = {
    getState(fromState).transitions.asInstanceOf[mutable.ListBuffer[Transition[Tok]]].append(EpsilonTransit(getState(toState)))
    this
  }
  def build:Automaton[Tok]  = {
    require(initialState.isDefined, "No initial getState defined")
    Automaton(initialState.get)
  }
}

