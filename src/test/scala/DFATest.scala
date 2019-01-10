import compiler.Compiler.State
import compiler.DFA
import exceptions.TransitionNonExistentException
import org.scalatest.FunSuite;

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

  def transition(state: State, alpha: Char): State = (state, alpha) match {
    case ("zero", '0')  => "one"
    case ("zero", '1')  => "two"
    case ("two", '0')   => "four"
    case ("two", '1')   => "three"
    case ("three", '0') => "three"
    case ("three", '1') => "two"
    case ("four", '0')  => "two"
    case ("four", '1')  => "four"
    case _ => throw TransitionNonExistentException()
  }

  test("Test single state transition") {
    val dfa = new DFA(states, accepting, "zero", alphabet, transition)
    val newDfa = dfa.next('0')
    assert(newDfa.getStartState().equals("one"))
  }

  test("Correctly identifies complete state") {
    val dfa = new DFA(states, accepting, "zero", alphabet, transition)
    assert(!dfa.isComplete())

    val newDfa = dfa.next('0')
    assert(newDfa.isComplete())
  }

  test("Transition failure") {
    val dfa = new DFA(states, accepting, "zero", alphabet, transition)
    assertThrows[TransitionNonExistentException] {
      dfa.next('2')
    }
  }
}
