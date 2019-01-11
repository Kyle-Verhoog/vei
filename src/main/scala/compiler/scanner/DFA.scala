package compiler

import compiler.Compiler.State
import exceptions.TransitionNonExistentException

import scala.collection.mutable

class DFA[T](val states: Set[State],
             val acceptingStates: Set[State],
             val startState: State,
             val alphabet: Set[T],
             val transitionTable: mutable.HashMap[(State, T), State]) {

  def transition(state: State, alpha: T): State = {
    if (!transitionTable.contains((state, alpha))) {
      throw TransitionNonExistentException()
    }
    transitionTable((state, alpha))
  }

  def next(alpha: T): DFA[T] = {
    new DFA(states,
            acceptingStates,
            transition(startState, alpha),
            alphabet,
            transitionTable)
  }

  def isComplete(): Boolean = {
    acceptingStates.contains(startState)
  }
}
