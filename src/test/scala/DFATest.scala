import compiler.Compiler.State
import compiler.DFA
import compiler.scanner.Token.Token
import exceptions.TransitionNonExistentException
import org.scalatest.FunSuite

import scala.collection.mutable

class DFATest extends FunSuite {
  val states = Set(
    "zero",
    "one",
    "two",
    "three",
    "four"
  )

  val accepting = Set(
    "one",
    "three"
  )

  val startState = "zero"

  val alphabet = Set('0', '1')

  val transitionTable = mutable.HashMap(
    ("zero", '0') -> "one",
    ("zero", '1') -> "two",
    ("two", '0') -> "four",
    ("two", '1') -> "three",
    ("three", '0') -> "three",
    ("three", '1') -> "two",
    ("four", '0') -> "two",
    ("four", '1') -> "four"
  )

  val tokenStates: mutable.HashMap[State, Token] = mutable.HashMap()

  test("Test single state transition") {
    val dfa = new DFA(states, accepting, "zero", alphabet, transitionTable, tokenStates)
    val newDfa = dfa.next('0')
    assert(newDfa.startState.equals("one"))
  }

  test("Correctly identifies complete state") {
    val dfa = new DFA(states, accepting, "zero", alphabet, transitionTable, tokenStates)
    assert(!dfa.isComplete())

    val newDfa = dfa.next('0')
    assert(newDfa.isComplete())
  }

  test("Transition failure") {
    val dfa = new DFA(states, accepting, "zero", alphabet, transitionTable, tokenStates)
    assertThrows[TransitionNonExistentException] {
      dfa.next('2')
    }
  }
}
