package compiler

import compiler.Compiler.State
import compiler.scanner.{Cache, Token}
import exceptions.TransitionNonExistentException

import scala.collection.{SortedSet, mutable}
import scala.util.Random

class NFA[T](
    val states: Set[State],
    val acceptingStates: Set[State],
    val startStates: Set[State],
    val alphabet: Set[T],
    val transitionTable: mutable.HashMap[(State, T), Set[State]],
    // stores accepting states that are related to a token
    val tokenStates: mutable.HashMap[State, Token], // TODO this should just be apart of `State`?
    val epsilonSym: T
) {

  def transition(state: State, alpha: T): Set[State] = {
    if (!transitionTable.contains((state, alpha))) {
      throw TransitionNonExistentException(message = "($state, $alpha)")
    }
    transitionTable((state, alpha))
  }

  def next(alpha: T): NFA[T] = {
    val nextStates = findNextStates(startStates, alpha)

    if (nextStates.isEmpty) {
      throw TransitionNonExistentException(
        message = s"No next states for $alpha"
      )
    }

    new NFA(
      states,
      acceptingStates,
      nextStates,
      alphabet,
      transitionTable,
      tokenStates,
      epsilonSym
    )
  }

  def findNextStates(currentStates: Set[State], alpha: T): Set[State] = {
    val temp = System.currentTimeMillis()
    val epsilonClosure = findEpsilonClosureMultipleStates(currentStates)
    //println("               time to find epsilon " + (System.currentTimeMillis() - temp))
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
      closureStates: Set[State] = Set[State]()
  ): Set[State] = {
    if (Cache.findEpsilonClosureCached(currentState, closureStates) != null) {
      return Cache.findEpsilonClosureCached(currentState, closureStates)
    }

    var newClosureStates = mutable.Set[State]()
    var queue: mutable.Queue[State] = mutable.Queue(currentState)

    while (queue.nonEmpty) {
      val dequedState = queue.dequeue()
      newClosureStates += dequedState

      try {
        for (state <- transition(dequedState, epsilonSym)) {
          if (!newClosureStates.contains(state)) {
            queue += state
          }
        }
      } catch {
        case e: TransitionNonExistentException => {}
      }
    }

    Cache.insert(currentState, closureStates, newClosureStates.toSet)
    newClosureStates.toSet
  }

  /*
  def findEpsilonClosure(
                          currentState: State,
                          closureStates: Set[State] = Set[State]()
                        ): Set[State] = {
    if (Cache.findEpsilonClosureCached(currentState) != null) {
      return Cache.findEpsilonClosureCached(currentState)
    }

    var newClosureStates = closureStates + currentState // should always include current state
    try {
      for (state <- transition(currentState, epsilonSym)) {
        if (!newClosureStates.contains(state)) {
          if (Cache.findEpsilonClosureCached(state) != null) {
            newClosureStates = Cache.findEpsilonClosureCached(state)
          } else {
            newClosureStates = findEpsilonClosure(state, newClosureStates)
          }
        }
      }
    } catch {
      case e: TransitionNonExistentException => {}
    }
    Cache.insert(currentState, newClosureStates.toSet)
    newClosureStates.toSet
  }
   */

  def findEpsilonClosureMultipleStates(
      currentStates: Set[State],
      closureStates: Set[State] = Set[State]()
  ): Set[State] = {
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

  def toDFA(): DFA[T] = {
    val nfaStartState = findEpsilonClosureMultipleStates(startStates)

    val startStateName = genDfaStateName(nfaStartState)
    val transitionList = mutable.HashMap[(String, T), String]()
    var states = Set[State](startStateName)
    var accepting = Set[State]()
    var newTokenStates = mutable.HashMap[State, Token]()

    // generate dfa.txt
    val queue = new mutable.Queue[Set[State]]
    queue.enqueue(nfaStartState)

    var prevTime = System.currentTimeMillis()

    while (queue.nonEmpty) {
      println("Processing NFA queue, at size: " + queue.length)
      println(
        "Time since last iteration: " + (System.currentTimeMillis() - prevTime))
      prevTime = System.currentTimeMillis()

      val currentState = queue.dequeue()
      val currentStateName = genDfaStateName(currentState)

      // check if we should add the current state to accepting
      if (isAccepting(currentState, acceptingStates)) {
        // add all token states that are not already defined
        currentState.foreach(state => {
          if (tokenStates.contains(state))
            newTokenStates += currentStateName -> tokenStates(state)
        })

        // add to accepting
        accepting += currentStateName
      }

      /*
       * Go through each possible transition and calculate the next states and that sets epsilon clousre
       * Then check if we already reached each of those states, if not add them to our states, and add
       * them to the queue
       */
      println(
        "Time to get to alphabet " + (System.currentTimeMillis() - prevTime))
      val alphaLoopTime = System.currentTimeMillis()

      for (alpha <- alphabet) {
        //println("finding next states and epsilon clouser " + alpha)
        val nextStates = findNextStates(currentState, alpha)
        //println("Time to find next states " + (System.currentTimeMillis() - prevTime))
        //println("----")
        val nextStatesEpsilonClosure = findEpsilonClosureMultipleStates(
          nextStates
        )
        //println("Time to find epsilon " + (System.currentTimeMillis() - prevTime))

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
      println("time to finish alpha " + (System.currentTimeMillis() - prevTime))
    }

    var dfa = new DFA(
      states,
      accepting,
      startStateName,
      alphabet,
      transitionList,
      newTokenStates
    )
    // TODO: HACK
    // NFA -> DFA accepting states in NFA should be in DFA
    // it seems like it's possible that accepting states don't have entries added to the tokenStates map
    dfa.tokenStates foreach {
      case (stok, token) => {
        dfa.transitionTable foreach {
          case ((stra, ch), out) => {
            if (stra contains stok) {
              dfa.tokenStates += (stra -> token)
            }
          }
        }
      }
    }
    dfa
  }

  def addTransitions(k: (State, T), v: Set[State]): NFA[T] = {
    var transitions = transitionTable
    var stateTransitions = transitions.get(k).getOrElse(Set[State]())
    var newStateTransitions = v | stateTransitions
    transitions += (k -> newStateTransitions)

    new NFA(
      states,
      acceptingStates,
      startStates,
      alphabet,
      transitionTable,
      tokenStates,
      epsilonSym
    )
  }

  def removeTransitions(k: (State, T), r: Set[State]): NFA[T] = {
    var transitions = transitionTable
    var stateTransitions = transitions.get(k).getOrElse(Set[State]())
    var newStateTransitions = stateTransitions diff r
    transitions += (k -> newStateTransitions)

    new NFA(
      states,
      acceptingStates,
      startStates,
      alphabet,
      transitionTable,
      tokenStates,
      epsilonSym
    )
  }

  override def toString = {
    s"""NFA(
      Q (states): $states
      q₀(startStates): $startStates
      F (acceptingStates): $acceptingStates
      Δ (transitionTable): $transitionTable
      Σ (alphabet): $alphabet
      T (tokenStates): $tokenStates
    )"""
  }
}
