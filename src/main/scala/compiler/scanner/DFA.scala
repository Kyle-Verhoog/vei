package compiler.scanner

import compiler.Compiler.State

class DFA[T](val states: Set[State],
             val acceptingStates: Set[State],
             val startState: State,
             val alphabet: Set[T],
             val transition: (State, T) => State) {

  def next(alpha: T): DFA[T] = {
    new DFA(states,
            acceptingStates,
            transition(startState, alpha),
            alphabet,
            transition)
  }

  def isComplete(): Boolean = {
    acceptingStates.contains(startState)
  }


}
