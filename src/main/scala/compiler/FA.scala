package compiler

import compiler.Compiler.State

abstract class FA[T](states: Set[State],
                     acceptingStates: Set[State],
                     startState: State,
                     alphabet: Set[T],
                     transition: (State, T) => State) {

  def next(alpha: T): FA[T]

  def getStartState(): State = { // TODO
    startState
  }

  def isComplete(): Boolean = {
    acceptingStates.contains(startState)
  }
}
