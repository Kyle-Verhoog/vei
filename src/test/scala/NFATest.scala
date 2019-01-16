import compiler.NFA
import compiler.scanner.Token
import org.scalatest.FunSuite

import scala.collection.mutable

class NFATest extends FunSuite {
  val states = Set(
    Set(1),
    Set(2),
    Set(3),
    Set(4),
  )

  val accepting = Set(
    Set(3),
    Set(4),
  )

  val startState = Set(1)

  val alphabet = Set('0', '1')

  val transitionTable = collection.mutable.HashMap(
    (Set(1), '0') -> Set(Set(2)),
    (Set(1), 'ε') -> Set(Set(3)),
    (Set(2), '1') -> Set(Set(2), Set(4)),
    (Set(3), 'ε') -> Set(Set(2)),
    (Set(3), '0') -> Set(Set(4)),
    (Set(4), '0') -> Set(Set(3))
  )

  val tokenStates: mutable.HashMap[NFA.T, Token] = mutable.HashMap()

  test("Test next with epsilon closure") {
    val nfa = new NFA(
      states,
      accepting,
      startStates=Set(Set(1)),
      alphabet,
      transitionTable,
      tokenStates,
      'ε'
    )
    val newNfa = nfa.next('1')

    assert(newNfa.startStates.diff(Set(Set(2), Set(4))).isEmpty)
  }

  test("Test epsilon closure") {
    val nfa = new NFA(
      states,
      accepting,
      Set(Set(1)),
      alphabet,
      transitionTable,
      tokenStates,
      'ε'
    )
    val epsilonClosure = nfa.findEpsilonClosure(Set(1))

    assert(epsilonClosure.diff(Set(Set(1), Set(3), Set(2))).isEmpty)
  }

  /*
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
      collection.mutable.HashMap[(NFA.T, String), Set[NFA.T]](),
      collection.mutable.HashMap[NFA.T, Token](),
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
      collection.mutable.HashMap[(NFA.T, String), Set[NFA.T]](),
      collection.mutable.HashMap[NFA.T, Token](),
      "ε"
    )

    var newNfa = nfa.addTransitions(("a", "b"), Set("b", "c"))
    assert(newNfa.transitionTable("a", "b") contains "b")
    assert(newNfa.transitionTable("a", "b") contains "c")
  }

  */
}

