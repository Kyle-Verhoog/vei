package compiler

import compiler.Compiler.State

class DFA[T](states: Set[State],
             acceptingStates: Set[State],
             startState: State,
             alphabet: Set[T],
             transition: (State, T) => State) {

  def next(alpha: T): DFA[T] = {
    new DFA(states,
            acceptingStates,
            transition(startState, alpha),
            alphabet,
            transition)
  }

  def getStartState(): State = { // TODO
    startState
  }

  def isComplete(): Boolean = {
    acceptingStates.contains(startState)
  }
}
