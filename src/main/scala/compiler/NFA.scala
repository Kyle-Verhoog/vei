package compiler

import compiler.Compiler.State

class NFA[T](states: Set[State],
             acceptingStates: Set[State],
             startState: State,
             alphabet: Set[T],
             transition: (State, T) => State)
    extends FA[T](states, acceptingStates, startState, alphabet, transition) {

  // TODO make NFA
  def next(alpha: T): FA[T] = {
    new NFA(states,
      acceptingStates,
      transition(startState, alpha),
      alphabet,
      transition)
  }
}
