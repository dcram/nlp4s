package fr.dcram.nlp4s.automata

import scala.collection.mutable

class AutomatonBuilder[Tok]() {

  abstract class TransitionDef[Tok](val target:StateDef[Tok]) {
    def asTransition:Transition[Tok]
  }
  case class MatcherTransitionDef[Tok](
                       matcher:TokenMatcher[Tok],
                       override val target:StateDef[Tok]) extends TransitionDef(target) {
    override def asTransition: Transition[Tok] = MatcherTransit(matcher, target.asState)
  }
  case class EpsilonTransitionDef[Tok](override val target:StateDef[Tok]) extends TransitionDef(target) {
    override def asTransition: Transition[Tok] = EpsilonTransit(target.asState)
  }

  case class AutTransitionDef[Tok](name:String, t:Automaton[Tok], override val target:StateDef[Tok]) extends TransitionDef(target){
    override def asTransition: Transition[Tok] = AutTransit(Some(name),t,target.asState)
  }
  case class TransitionableTransitionDef[Tok](t:Transitionable[Tok], override val target:StateDef[Tok]) extends TransitionDef(target) {
    override def asTransition: Transition[Tok] = t.asTransition(target.asState)
  }

  class StateDef[Tok](
                       val id:Int,
                       val transitions:mutable.ListBuffer[TransitionDef[Tok]],
                       accepting:Boolean) {
    def asState:State[Tok] = State(id, () => transitions.toStream.map(_.asTransition), accepting)
  }

  private[this] var initialState:Option[StateDef[Tok]] = None
  private[this] val states = new mutable.HashMap[Int, StateDef[Tok]]()
  private[this] def getStateDef(id:Int):StateDef[Tok] = states(id)
  def initialState(id:Int):AutomatonBuilder[Tok] = {
    require(initialState.isEmpty, "Initial state already defined")
    state(id)
    initialState = Some(getStateDef(id))
    this
  }
  def state(id:Int, accepting:Boolean = false):AutomatonBuilder[Tok] = {
    require(!states.contains(id), s"State $id already defined")
    states.put(id, new StateDef[Tok](id, new mutable.ListBuffer[TransitionDef[Tok]](), accepting))
    this
  }
  def matcherTransition(fromState: Int, toState:Int, matcher:TokenMatcher[Tok]):AutomatonBuilder[Tok] = {
    val transition = new MatcherTransitionDef[Tok](matcher, getStateDef(toState))
    getStateDef(fromState).transitions.append(transition)
    this
  }
  def transition(fromState: Int, toState:Int, t:Transitionable[Tok]):AutomatonBuilder[Tok] = {
    getStateDef(fromState).transitions.append(TransitionableTransitionDef(t, getStateDef(toState)))
    this
  }
  def namedTransition(name:String, fromState: Int, toState:Int, t:Automaton[Tok]):AutomatonBuilder[Tok] = {
    getStateDef(fromState).transitions.append(AutTransitionDef(name, t, getStateDef(toState)))
    this
  }
  def epsilon(fromState: Int, toState:Int):AutomatonBuilder[Tok] = {
    getStateDef(fromState).transitions.append(EpsilonTransitionDef(getStateDef(toState)))
    this
  }
  def build:Automaton[Tok]  = {
    require(initialState.isDefined, "No initial getStateDef defined")
    Automaton(initialState.get.asState)
  }
}

