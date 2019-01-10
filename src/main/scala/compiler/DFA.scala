package compiler

import compiler.Compiler.State

class DFA(states: Set[State],
          acceptingStates: Set[State],
          startState: State,
          alphabet: Set[Char],
          transition: (State, Char) => State) {

  def next(alpha: Char): DFA = {
    new DFA(states, acceptingStates, transition(startState, alpha), alphabet, transition)
  }

  def getStartState() : State = { // TODO
    startState
  }

  def isComplete() : Boolean = {
    acceptingStates.contains(startState)
  }
}
