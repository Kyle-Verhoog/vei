package compiler.scanner

import compiler.scanner.Token
import exceptions.TransitionNonExistentException
import org.scalatest.FunSuite

import scala.collection.mutable

class DFATest extends FunSuite {
  val states = Set(
    Set(0),
    Set(1),
    Set(2),
    Set(3),
    Set(4)
  )

  val accepting = Set(
    Set(1),
    Set(3)
  )

  val startState = Set(0)

  val alphabet = Set('0', '1')

  val transitionTable = mutable.HashMap(
    (Set(0), '0') -> Set(1),
    (Set(0), '1') -> Set(2),
    (Set(2), '0') -> Set(4),
    (Set(2), '1') -> Set(3),
    (Set(3), '0') -> Set(3),
    (Set(3), '1') -> Set(2),
    (Set(4), '0') -> Set(2),
    (Set(4), '1') -> Set(4)
  )

  val tokenStates: mutable.HashMap[NFA.T, Token] = mutable.HashMap()

  test("Test single state transition") {
    val dfa =
      new DFA(states, accepting, Set(0), alphabet, transitionTable, tokenStates)
    val newDfa = dfa.next('0')
    assert(newDfa.startState.equals(Set(1)))
  }

  test("Correctly identifies complete state") {
    val dfa =
      new DFA(states, accepting, Set(0), alphabet, transitionTable, tokenStates)
    assert(!dfa.isComplete())

    val newDfa = dfa.next('0')
    assert(newDfa.isComplete())
  }

  test("Transition failure") {
    val dfa =
      new DFA(states, accepting, Set(0), alphabet, transitionTable, tokenStates)
    assertThrows[TransitionNonExistentException] {
      dfa.next('2')
    }
  }
}
