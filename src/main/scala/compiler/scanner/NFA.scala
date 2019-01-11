package compiler.scanner

import compiler.Compiler.State
import exceptions.TransitionNonExistentException

import scala.collection.{SortedSet, mutable}

class NFA[T](val states: Set[State],
             val acceptingStates: Set[State],
             val startState: Set[State],
             val alphabet: Set[T],
             val transition: (State, T) => Set[State],
             val epsilonSym: T) {

  def next(alpha: T): NFA[T] = {
    val nextStates = findNextStates(startState, alpha)

    if (nextStates.isEmpty) {
      throw TransitionNonExistentException()
    }

    new NFA(states,
            acceptingStates,
            nextStates,
            alphabet,
            transition,
            epsilonSym)
  }

  def findNextStates(currentState: Set[State], alpha: T): Set[State] = {
    val epsilonClosure = findEpsilonClosureMultipleStates(currentState)
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
      currentState: Set[State],
      closureStates: Set[State] = Set[State]()): Set[State] = {
    var epsilonClosure = currentState
    for (state <- currentState) {
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
    isAccepting(startState, acceptingStates)
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
    val nfaStartState = findEpsilonClosureMultipleStates(startState)

    val startStateName = genDfaStateName(nfaStartState)
    val transitionList = mutable.HashMap[(String, T), String]()
    var states = Set[State](startStateName)
    var accepting = Set[State]()

    // generate dfa
    val queue = new mutable.Queue[Set[State]]
    queue.enqueue(nfaStartState)

    while (queue.nonEmpty) {
      val currentState = queue.dequeue()
      val currentStateName = genDfaStateName(currentState)

      // check if we should add the current state to accepting
      if (isAccepting(currentState, acceptingStates)) {
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
          // check if we already have this state
          if (!states.contains(stateName)) {
            states += stateName
            queue.enqueue(nextStatesEpsilonClosure)
            transitionList += ((currentStateName, alpha) -> stateName)
          }
        }
      }
    }

    new DFA(
      states,
      accepting,
      startStateName,
      alphabet,
      (state: State, alpha: T) => {
        if (!transitionList.contains((state, alpha))) {
          throw TransitionNonExistentException()
        }
        transitionList((state, alpha))
      }
    )
  }
}
