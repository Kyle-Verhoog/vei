package compiler

import compiler.Compiler.State

class DFA[T](states: Set[State],
             acceptingStates: Set[State],
             startState: State,
             alphabet: Set[T],
             transition: (State, T) => State)
  extends FA[T](states, acceptingStates, startState, alphabet, transition) {

  def next(alpha: T): FA[T] = {
    new NFA(states,
      acceptingStates,
      transition(startState, alpha),
      alphabet,
      transition)
  }
}
