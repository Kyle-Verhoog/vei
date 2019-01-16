package compiler

import compiler.scanner.Token

import scala.collection.mutable

/*
object Utility {
  def postfixStates(states: Set[State], postfix: String): Set[State] = {
    states.map(state => postfixState(state, postfix))
  }

  def postfixState(state: State, postfix: String): State = {
    state + postfix
  }

  def postfixTransitionTable[T](
      table: mutable.HashMap[(State, T), Set[State]],
      postfix: String
  ): mutable.HashMap[(State, T), Set[State]] = {
    val newTable = mutable.HashMap[(State, T), Set[State]]()

    for (key <- table.keySet) {
      val newKey = (postfixState(key._1, postfix), key._2)
      val newValue = postfixStates(table(key), postfix)
      newTable += (newKey -> newValue)
    }

    newTable
  }

  def postfixTokenStates[T](
      table: mutable.HashMap[State, Token],
      postfix: String
  ): mutable.HashMap[State, Token] = {
    val newTable = mutable.HashMap[State, Token]()
    for (key <- table.keySet) {
      newTable += (postfixState(key, postfix) -> table(key))
    }

    newTable
  }

  def merge[T](nfas: Set[NFA[T]], epsilonSym: T): NFA[T] = {
    var states = Set[State]()
    var startStates = Set[State]()
    var acceptingStates = Set[State]()
    var alphabet = Set[T]()
    var transitionTable = mutable.HashMap[(State, T), Set[State]]()
    var tokenStates = mutable.HashMap[State, Token]()

    // TODO what if epsilon symbols are different? Do we care to check this

    // add postfix version of NFA to the new one
    var i = 0
    for (nfa <- nfas) {
      i += 1
      val postfix = "MERGED_POSTFIX" + i
      states = states.union(postfixStates(nfa.states, postfix))
      startStates = startStates.union(postfixStates(nfa.startStates, postfix))
      acceptingStates =
        acceptingStates.union(postfixStates(nfa.acceptingStates, postfix))
      alphabet = alphabet.union(nfa.alphabet)
      transitionTable ++= postfixTransitionTable(nfa.transitionTable, postfix)
      tokenStates ++= postfixTokenStates(nfa.tokenStates, postfix)
    }

    new NFA[T](
      states,
      acceptingStates,
      startStates,
      alphabet,
      transitionTable,
      tokenStates,
      epsilonSym
    )
  }

  def runDfa(input: String, dfa: DFA[State]): mutable.MutableList[Token] = {
    var currentDfa = dfa
    var tokens = mutable.MutableList[Token]()

    val it = input.toIterator.buffered
    while (it.hasNext) {
      val currentChar = it.next().toString
      if (!currentChar.equals(" ")) {
        currentDfa = currentDfa.next(currentChar)

        // Stop if complete, and cant proceed
        if (currentDfa.isComplete() && !(it.hasNext && currentDfa.canProceed(
              it.head.toString
            ))) {
          tokens += currentDfa.getCurrentToken()
          currentDfa = dfa
        }
      }
    }

    tokens
  }
}
*/
