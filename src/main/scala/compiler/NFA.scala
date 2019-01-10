package compiler

import compiler.Compiler.State
import exceptions.TransitionNonExistentException

class NFA[T](states: Set[State],
             acceptingStates: Set[State],
             startState: Set[State],
             alphabet: Set[T],
             transition: (State, T) => Set[State],
             epsilonSym: T) {

  // TODO make NFA
  /*def next(alpha: T): NFA[T] = {
    new NFA(states,
            acceptingStates,
            transition(startState, alpha),
            alphabet,
            transition)

  }
   */

  def findEpsilonClosure(currentState: State,
                         closureStates: Set[State]): Set[State] = {
    var newClosureStates = closureStates + currentState // should always include current state
    try {
      for (state <- transition(currentState, epsilonSym)) {
        if (!closureStates.contains(state)) {
          newClosureStates = findEpsilonClosure(state, newClosureStates)
        }
      }
    } catch {
      case e: TransitionNonExistentException => {}
    }

    newClosureStates
  }

  def findEpsilonClosure(currentState: Set[State],
                         closureStates: Set[State]): Set[State] = {
    var epsilonClosure = currentState
    for (state <- currentState) {
      epsilonClosure =
        epsilonClosure.union(findEpsilonClosure(state, closureStates))
    }
    epsilonClosure
  }

  def getStartState(): Set[State] = { // TODO
    startState
  }

  def isComplete(): Boolean = {
    for (state <- startState) {
      if (acceptingStates.contains(state)) {
        return true
      }
    }
    false
  }
}
