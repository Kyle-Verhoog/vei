package compiler

import compiler.Compiler.State
import compiler.scanner.Token.Token
import exceptions.TransitionNonExistentException

import scala.collection.{SortedSet, mutable}

class NFA[T](val states: Set[State],
             val acceptingStates: Set[State],
             val startStates: Set[State],
             val alphabet: Set[T],
             val transitionTable: mutable.HashMap[(State, T), Set[State]],
             // stores accepting states that are related to a token
             val tokenStates: mutable.HashMap[State, Token],
             val epsilonSym: T) {

  def transition(state: State, alpha: T): Set[State] = {
    if (!transitionTable.contains((state, alpha))) {
      throw TransitionNonExistentException()
    }
    transitionTable((state, alpha))
  }

  def next(alpha: T): NFA[T] = {
    val nextStates = findNextStates(startStates, alpha)

    if (nextStates.isEmpty) {
      throw TransitionNonExistentException()
    }

    new NFA(states,
            acceptingStates,
            nextStates,
            alphabet,
            transitionTable,
            tokenStates,
            epsilonSym)
  }

  def findNextStates(currentStates: Set[State], alpha: T): Set[State] = {
    val epsilonClosure = findEpsilonClosureMultipleStates(currentStates)
    var newStates = Set[State]()

    for (state <- epsilonClosure) {
      try {
        newStates = newStates.union(transition(state, alpha))
      } catch {
        case e: TransitionNonExistentException => {}
      }
    }

    newStates
  }

  def findEpsilonClosure(
      currentState: State,
      closureStates: Set[State] = Set[State]()): Set[State] = {
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

  def findEpsilonClosureMultipleStates(
      currentStates: Set[State],
      closureStates: Set[State] = Set[State]()): Set[State] = {
    var epsilonClosure = currentStates
    for (state <- currentStates) {
      epsilonClosure =
        epsilonClosure.union(findEpsilonClosure(state, closureStates))
    }
    epsilonClosure
  }

  def isAccepting(states: Set[State], accepting: Set[State]): Boolean = {
    for (state <- states) {
      if (accepting.contains(state)) {
        return true
      }
    }
    false
  }

  def isComplete(): Boolean = {
    isAccepting(startStates, acceptingStates)
  }

  def genDfaStateName(states: Set[State]): String = {
    if (states.isEmpty) {
      throw new RuntimeException("DFA state name cannot be empty!")
    }
    var sorted = SortedSet[State]()
    sorted = sorted.union(states)

    sorted.map(state => state.toString).mkString("")
  }

  def createDfa(): DFA[T] = {
    val nfaStartState = findEpsilonClosureMultipleStates(startStates)

    val startStateName = genDfaStateName(nfaStartState)
    val transitionList = mutable.HashMap[(String, T), String]()
    var states = Set[State](startStateName)
    var accepting = Set[State]()
    var newTokenStates = mutable.HashMap[State, Token]()

    // generate dfa
    val queue = new mutable.Queue[Set[State]]
    queue.enqueue(nfaStartState)

    while (queue.nonEmpty) {
      val currentState = queue.dequeue()
      val currentStateName = genDfaStateName(currentState)

      // check if we should add the current state to accepting
      if (isAccepting(currentState, acceptingStates)) {
        // add all token states that are not already defined
        currentState.foreach(state => {
          // TODO determine if we want to break ties here in some way
          if (!newTokenStates.contains(state) && tokenStates.contains(state))
            newTokenStates += state -> tokenStates(state)
        })

        // add to accepting
        accepting += currentStateName
      }

      /*
       * Go through each possible transition and calculate the next states and that sets epsilon clousre
       * Then check if we already reached each of those states, if not add them to our states, and add
       * them to the queue
       */
      for (alpha <- alphabet) {
        val nextStates = findNextStates(currentState, alpha)
        val nextStatesEpsilonClosure = findEpsilonClosureMultipleStates(
          nextStates)

        // NOTE: apparently scala has no `continue` so this is what i had to do
        if (nextStatesEpsilonClosure.nonEmpty) {
          val stateName = genDfaStateName(nextStatesEpsilonClosure)
          transitionList += ((currentStateName, alpha) -> stateName)

          // check if we already have this state before enqueing it again
          if (!states.contains(stateName)) {
            states += stateName
            queue.enqueue(nextStatesEpsilonClosure)
          }
        }
      }
    }

    new DFA(
      states,
      accepting,
      startStateName,
      alphabet,
      transitionList,
      newTokenStates
    )
  }
}
