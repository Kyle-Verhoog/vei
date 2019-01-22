package compiler.scanner

import exceptions.TransitionNonExistentException

import scala.collection.mutable

object NFA {
  type T = Set[Int]
  var idCount = 0

  def genId(): Int = {
    val r = idCount
    idCount += 1
    r
  }

  // all ASCII chars
  val charAlphabet = (0 to 127).map(i => i.toChar.toString).toSet

  def newStringNFA(
      states: Set[NFA.T],
      acceptingStates: Set[NFA.T],
      startStates: Set[NFA.T],
      transitionTable: mutable.HashMap[(NFA.T, String), Set[NFA.T]]
  ): NFA[String] = {
    new NFA[String](
      states,
      acceptingStates,
      startStates,
      charAlphabet,
      transitionTable,
      mutable.HashMap[NFA.T, Token](),
      "ε"
    )
  }

  def newState(prevStates: Set[T] = Set[T]()): T = {
    var state = Set(genId())
    for (prev <- prevStates) {
      state = state union prev
    }
    state
  }

  def foldStates(prevStates: Set[T] = Set[T]()): T = {
    var s = Set[Int]()
    for (prev <- prevStates) {
      s = s union prev
    }
    s
  }
}

class NFA[TTrans](
    val states: Set[NFA.T],
    val acceptingStates: Set[NFA.T],
    val startStates: Set[NFA.T],
    val alphabet: Set[TTrans],
    val transitionTable: mutable.HashMap[(NFA.T, TTrans), Set[NFA.T]],
    // stores accepting states that are related to a token
    val tokenStates: mutable.HashMap[NFA.T, Token],
    val epsilonSym: TTrans
) {

  def transition(state: NFA.T, alpha: TTrans): Set[NFA.T] = {
    if (!transitionTable.contains((state, alpha))) {
      throw TransitionNonExistentException(message = "($state, $alpha)")
    }
    transitionTable((state, alpha))
  }

  def next(alpha: TTrans): NFA[TTrans] = {
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

  def findNextStates(currentStates: Set[NFA.T], alpha: TTrans): Set[NFA.T] = {
    val epsilonClosures = findEpsilonClosureMultipleStates(currentStates)
    var newStates = Set[NFA.T]()

    for (state <- epsilonClosures) {
      try {
        newStates = newStates.union(transition(state, alpha))
      } catch {
        case e: TransitionNonExistentException => {}
      }
    }

    newStates
  }

  def findEpsilonClosure(
      currentState: NFA.T,
      closureStates: Set[NFA.T] = Set[NFA.T]()
  ): Set[NFA.T] = {
    if (Cache.findEpsilonClosureCached(currentState, closureStates) != null) {
      return Cache.findEpsilonClosureCached(currentState, closureStates)
    }

    var newClosureStates = mutable.Set[NFA.T]()
    var queue: mutable.Queue[NFA.T] = mutable.Queue(currentState)

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
        case _: TransitionNonExistentException =>
      }
    }

    Cache.insert(currentState, closureStates, newClosureStates.toSet)
    newClosureStates.toSet
  }

  def findEpsilonClosureMultipleStates(
      currentStates: Set[NFA.T],
      closureStates: Set[NFA.T] = Set[NFA.T]()
  ): Set[NFA.T] = {
    var epsilonClosure = currentStates
    for (state <- currentStates) {
      epsilonClosure =
        epsilonClosure.union(findEpsilonClosure(state, closureStates))
    }
    epsilonClosure
  }

  def isAccepting(states: Set[NFA.T], accepting: Set[NFA.T]): Boolean = {
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

  def toDFA(): DFA[TTrans] = {
    val nfaStartState = findEpsilonClosureMultipleStates(startStates)
    val startState = NFA.foldStates(nfaStartState)
    val newTransitionTable = mutable.HashMap[(NFA.T, TTrans), NFA.T]()
    var states = Set[NFA.T](startState)
    var accepting = Set[NFA.T]()
    var newTokenStates = mutable.HashMap[NFA.T, Token]()

    val queue = new mutable.Queue[Set[NFA.T]]
    queue.enqueue(nfaStartState)

    while (queue.nonEmpty) {
      if (queue.length % 20 == 0)
        println("Processing NFA queue, at size: " + queue.length)

      val currentState = queue.dequeue()
      val newState = NFA.foldStates(currentState)

      states += newState

      // check if we should add the current state to accepting
      if (isAccepting(currentState, acceptingStates)) {
        // look through each old state that is creating the currentState
        currentState.foreach(state => {
          // if the old state is a token state add it
          if (tokenStates.contains(state)) {
            // only replace if replacing with smaller number (ordering matters)!!!!!!!!!
            if (newTokenStates.contains(newState)) {
              if (tokenStates(state).tokenNumber < newTokenStates(newState).tokenNumber) {
                newTokenStates += newState -> tokenStates(state)
              }
            } else {
              // otherwise just do it
              newTokenStates += newState -> tokenStates(state)
            }
          }
        })

        // add to accepting
        accepting += newState
      }

      /*
       * Go through each possible transition and calculate the next states and that sets epsilon closure
       * Then check if we already reached each of those states, if not add them to our states, and add
       * them to the queue
       */
      // println("Time to get to alphabet " + (System.currentTimeMillis() - prevTime))
      // val alphaLoopTime = System.currentTimeMillis()

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
          val state = NFA.foldStates(nextStatesEpsilonClosure)
          newTransitionTable += ((newState, alpha) -> state)

          // check if we already have this state before enqueing it again
          if (!states.contains(state)) {
            states += state
            queue.enqueue(nextStatesEpsilonClosure)
          }
        }
      }
      //println("time to finish alpha " + (System.currentTimeMillis() - prevTime))
    }

    var dfa = new DFA[TTrans](
      states,
      accepting,
      startState,
      alphabet,
      newTransitionTable,
      newTokenStates
    )

    // TODO: HACK
    // NFA -> DFA accepting states in NFA should be in DFA
    // it seems like it's possible that accepting states don't have entries added to the tokenStates map
    // dfa.tokenStates foreach {
    //   case (stoken, token) => {
    //     dfa.transitionTable foreach {
    //       case ((strans, ch), out) => {
    //         for (i <- stra) {
    //           println('o')
    //           if (!(stra intersect stok).isEmpty) {
    //             dfa.tokenStates += (stra -> token)
    //           }
    //         }
    //       }
    //     }
    //   }
    // }
    dfa
  }

  def addTransitions(k: (NFA.T, TTrans), v: Set[NFA.T]): NFA[TTrans] = {
    var transitions = transitionTable
    var stateTransitions = transitions.get(k).getOrElse(Set[NFA.T]())
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
