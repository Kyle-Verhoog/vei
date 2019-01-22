package compiler.scanner

import scala.collection.mutable

object Cache {
  val epsilonClosureCache = mutable.HashMap[(NFA.T, Set[NFA.T]), Set[NFA.T]]()

  def findEpsilonClosureCached(
      currentState: NFA.T,
      closureStates: Set[NFA.T] = Set[NFA.T]()
  ): Set[NFA.T] = {
    if (epsilonClosureCache.contains((currentState, closureStates))) {
      return epsilonClosureCache((currentState, closureStates))
    }
    null
  }

  def insert(
      currentState: NFA.T,
      closureStates: Set[NFA.T] = Set[NFA.T](),
      result: Set[NFA.T]
  ) = {
    epsilonClosureCache += ((currentState, closureStates) -> result)
  }

}
