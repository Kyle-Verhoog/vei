package compiler.scanner

import compiler.Compiler.State
import exceptions.TransitionNonExistentException

import scala.collection.mutable

object Cache {
  val epsilonClosureCache = mutable.HashMap[(State, Set[State]), Set[State]]()

  def findEpsilonClosureCached(
                                currentState: State,
                                closureStates: Set[State] = Set[State]()
                              ): Set[State] = {
    if (epsilonClosureCache.contains((currentState, closureStates))) {
      return epsilonClosureCache((currentState, closureStates))
    }
    null
  }

  def insert(
                                currentState: State,
                                closureStates: Set[State] = Set[State](),
            result: Set[State]) = {
    epsilonClosureCache += ((currentState, closureStates) -> result)
  }

}
