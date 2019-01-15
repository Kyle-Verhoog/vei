package compiler

import compiler.Compiler.State
import compiler.scanner.Token
import exceptions.TransitionNonExistentException
import exceptions.NoTokenOnAcceptingStateException

import scala.collection.mutable

class DFA[T](
    val states: Set[State],
    val acceptingStates: Set[State],
    val startState: State,
    val alphabet: Set[T],
    val transitionTable: mutable.HashMap[(State, T), State],
    val tokenStates: mutable.HashMap[State, Token],
    val processedTransitions: Set[T] = Set[T]()
) {

  def transition(state: State, alpha: T): State = {
    if (!transitionTable.contains((state, alpha))) {
      throw TransitionNonExistentException(message = s"$state, $alpha")
    }
    transitionTable((state, alpha))
  }

  def next(alpha: T): DFA[T] = {
    new DFA(
      states,
      acceptingStates,
      transition(startState, alpha),
      alphabet,
      transitionTable,
      tokenStates,
      processedTransitions + alpha
    )
  }

  def isComplete(): Boolean = {
    acceptingStates.contains(startState)
  }

  def canProceed(alpha: T): Boolean = {
    transitionTable.contains((startState, alpha))
  }

  def getCurrentToken(): Token = {
    if (!tokenStates.contains(startState))
      throw NoTokenOnAcceptingStateException(message = s"Start State:\n$startState\nTransitions:\n$processedTransitions")
    tokenStates(startState)
  }

  override def toString = {
    s"""DFA(
      Q (states): $states
      q₀(startState): $startState
      F (acceptingStates): $acceptingStates
      Δ (transitionTable): $transitionTable
      Σ (alphabet): $alphabet
      T (tokenStates): $tokenStates
    )"""
  }
}
