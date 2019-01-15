import compiler.Compiler.State
import compiler.NFA
import compiler.scanner.Token
import org.scalatest.FunSuite

import scala.collection.mutable

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

  val tokenStates: mutable.HashMap[State, Token] = mutable.HashMap()

  test("Test next with epsilon closure") {
    val nfa = new NFA(
      states,
      accepting,
      Set("one"),
      alphabet,
      transitionTable,
      tokenStates,
      'ε'
    )
    val newNfa = nfa.next('1')

    assert(newNfa.startStates.diff(Set("two", "four")).isEmpty)
  }

  test("Test epsilon closure") {
    val nfa = new NFA(
      states,
      accepting,
      Set("one"),
      alphabet,
      transitionTable,
      tokenStates,
      'ε'
    )
    val epsilonClosure = nfa.findEpsilonClosure("one")

    assert(epsilonClosure.diff(Set("one", "three", "two")).isEmpty)
  }

  test("Test epsilon closure without epsilon transition") {
    val nfa = new NFA(
      states,
      accepting,
      Set("one"),
      alphabet,
      transitionTable,
      tokenStates,
      'ε'
    )
    val epsilonClosure = nfa.findEpsilonClosure("two")

    assert(epsilonClosure.diff(Set("two")).isEmpty)
  }

  test("Test convert to DFA") {
    val nfa = new NFA(
      states,
      accepting,
      Set("one"),
      alphabet,
      transitionTable,
      tokenStates,
      'ε'
    )
    val dfa = nfa.toDFA()

    assert(
      dfa.states
        .diff(Set("onethreetwo", "fourtwo", "threetwo", "four"))
        .isEmpty
    )
    assert(
      dfa.acceptingStates
        .diff(Set("onethreetwo", "fourtwo", "threetwo", "four"))
        .isEmpty
    )
    assert(dfa.startState.equals("onethreetwo"))
    // TODO determine how to test transitions

  }

  test("addTransitions one") {
    val nfa = new NFA(
      Set("a", "b"),
      Set("b"),
      Set("a"),
      Set("a", "b", "c"),
      collection.mutable.HashMap[(State, String), Set[State]](),
      collection.mutable.HashMap[State, Token](),
      "ε"
    )

    var newNfa = nfa.addTransitions(("a", "b"), Set("b"))
    assert(newNfa.transitionTable contains ("a", "b"))
    newNfa = nfa.addTransitions(("a", "ε"), Set("b"))
    assert(newNfa.transitionTable contains ("a", "b"))
    assert(newNfa.transitionTable contains ("a", "ε"))
  }

  test("addTransitions multi") {
    val nfa = new NFA(
      Set("a", "b"),
      Set("b"),
      Set("a"),
      Set("a", "b", "c"),
      collection.mutable.HashMap[(State, String), Set[State]](),
      collection.mutable.HashMap[State, Token](),
      "ε"
    )

    var newNfa = nfa.addTransitions(("a", "b"), Set("b", "c"))
    assert(newNfa.transitionTable("a", "b") contains "b")
    assert(newNfa.transitionTable("a", "b") contains "c")
  }


  test("removeTransitions one") {
    val nfa = new NFA(
      Set("a", "b"),
      Set("b"),
      Set("a"),
      Set("a", "b", "c"),
      collection.mutable.HashMap[(State, String), Set[State]](),
      collection.mutable.HashMap[State, Token](),
      "ε"
    )

    var newNfa = nfa.addTransitions(("a", "b"), Set("b", "c"))
    assert(newNfa.transitionTable("a", "b") contains "b")
    assert(newNfa.transitionTable("a", "b") contains "c")
    nfa.removeTransitions(("a", "b"), Set("b"))
    assert(!(newNfa.transitionTable("a", "b") contains "b"))
    assert(newNfa.transitionTable("a", "b") contains "c")
  }

  test("removeTransitions multi") {
    val nfa = new NFA(
      Set("a", "b"),
      Set("b"),
      Set("a"),
      Set("a", "b", "c"),
      collection.mutable.HashMap[(State, String), Set[State]](),
      collection.mutable.HashMap[State, Token](),
      "ε"
    )

    var newNfa = nfa.addTransitions(("a", "b"), Set("a", "b", "c"))
    assert(newNfa.transitionTable("a", "b") contains "a")
    assert(newNfa.transitionTable("a", "b") contains "b")
    assert(newNfa.transitionTable("a", "b") contains "c")
    nfa.removeTransitions(("a", "b"), newNfa.transitionTable("a", "b"))
    assert(!(newNfa.transitionTable("a", "b") contains "a"))
    assert(!(newNfa.transitionTable("a", "b") contains "b"))
    assert(!(newNfa.transitionTable("a", "b") contains "c"))
  }
}
