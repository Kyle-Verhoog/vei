package compiler

import compiler.scanner.{Token}
import exceptions.TransitionNonExistentException
import exceptions.NoTokenOnAcceptingStateException

import scala.collection.mutable

@SerialVersionUID(1001L)
class DFA[TTrans](
    val states: Set[NFA.T],
    val acceptingStates: Set[NFA.T],
    val startState: NFA.T,
    val alphabet: Set[TTrans],
    val transitionTable: mutable.HashMap[(NFA.T, TTrans), NFA.T],
    val tokenStates: mutable.HashMap[NFA.T, Token],
    val processedTransitions: Set[TTrans] = Set[TTrans]()
) extends Serializable {

  def transition(state: NFA.T, alpha: TTrans): NFA.T = {
    if (!transitionTable.contains((state, alpha))) {
      throw TransitionNonExistentException(message = s"$state, $alpha")
    }
    transitionTable((state, alpha))
  }

  def next(alpha: TTrans): DFA[TTrans] = {
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

  def canProceed(alpha: TTrans): Boolean = {
    transitionTable.contains((startState, alpha))
  }

  def getCurrentToken(): Token = {
    if (!tokenStates.contains(startState))
      throw NoTokenOnAcceptingStateException(
        message =
          s"Start State:\n$startState\nTransitions:\n$processedTransitions"
      )
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
