import compiler.Compiler.State
import compiler.{DFA, NFA}
import exceptions.TransitionNonExistentException
import org.scalatest.FunSuite;

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

  val alphabet = Set('0', '1', 'ε')

  def transition(state: State, alpha: Char): Set[State] = (state, alpha) match {
    case ("one", '0')  => Set("two")
    case ("one", 'ε')  => Set("three")
    case ("two", '1')  => Set("two", "four")
    case ("three", 'ε')  => Set("two")
    case ("three", '0')  => Set("four")
    case ("four", '0')  => Set("three")
    case _ => throw TransitionNonExistentException()
  }

  test("Test epsilon clousre") {
    val nfa = new NFA(states, accepting, Set("one"), alphabet, transition, 'ε')
    val epsilonClosure = nfa.findEpsilonClosure("one", Set[State]())

    assert(epsilonClosure.diff(Set("one", "three", "two")).isEmpty)
  }

  test("Test epsilon clousre without epsilon transition") {
    val nfa = new NFA(states, accepting, Set("one"), alphabet, transition, 'ε')
    val epsilonClosure = nfa.findEpsilonClosure("two", Set[State]())

    assert(epsilonClosure.diff(Set("two")).isEmpty)
  }
}
