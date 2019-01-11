import compiler.NFA
import org.scalatest.FunSuite

class NFATest extends FunSuite {
  val states = Set(
    "one",
    "two",
    "three",
    "four"
  )

  val accepting = Set(
    "three",
    "four"
  )

  val startState = "one"

  val alphabet = Set('0', '1')

  val transitionTable = collection.mutable.HashMap(
    ("one", '0') -> Set("two"),
    ("one", 'ε') -> Set("three"),
    ("two", '1') -> Set("two", "four"),
    ("three", 'ε') -> Set("two"),
    ("three", '0') -> Set("four"),
    ("four", '0') -> Set("three")
  )

  test("Test next with epsilon closure") {
    val nfa = new NFA(states, accepting, Set("one"), alphabet, transitionTable, 'ε')
    val newNfa = nfa.next('1')

    assert(newNfa.startStates.diff(Set("two", "four")).isEmpty)
  }

  test("Test epsilon clousre") {
    val nfa = new NFA(states, accepting, Set("one"), alphabet, transitionTable, 'ε')
    val epsilonClosure = nfa.findEpsilonClosure("one")

    assert(epsilonClosure.diff(Set("one", "three", "two")).isEmpty)
  }

  test("Test epsilon clousre without epsilon transition") {
    val nfa = new NFA(states, accepting, Set("one"), alphabet, transitionTable, 'ε')
    val epsilonClosure = nfa.findEpsilonClosure("two")

    assert(epsilonClosure.diff(Set("two")).isEmpty)
  }

  test("Test convert to DFA") {
    val nfa = new NFA(states, accepting, Set("one"), alphabet, transitionTable, 'ε')
    val dfa = nfa.createDfa()

    assert(
      dfa.states
        .diff(Set("onethreetwo", "fourtwo", "threetwo", "four"))
        .isEmpty)
    assert(
      dfa.acceptingStates
        .diff(Set("onethreetwo", "fourtwo", "threetwo", "four"))
        .isEmpty)
    assert(dfa.startState.equals("onethreetwo"))
    // TODO determine how to test transitions

  }
}
