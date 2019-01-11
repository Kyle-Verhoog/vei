import compiler.Compiler.State
import compiler.Utility
import compiler.NFA
import org.scalatest.FunSuite

import scala.collection.mutable

class ScannerUtilityTest extends FunSuite {
  def genNFA1(): NFA[State] = {
    val states = Set("one", "two")
    val accepting = Set("two")
    val startState = Set("one")
    val alphabet = Set("0", "1")
    val transitionTable: mutable.HashMap[(State, State), Set[State]] =
      mutable.HashMap(
        ("one", "0") -> Set("two"),
        ("two", "1") -> Set("two")
      )

    new NFA[State](states,
                   accepting,
                   startState,
                   alphabet,
                   transitionTable,
                   "ε")
  }

  def genNFA2(): NFA[State] = {
    val states = Set("three", "four")
    val accepting = Set("four")
    val startState = Set("three")
    val alphabet = Set("0", "1")
    val transitionTable: mutable.HashMap[(State, State), Set[State]] =
      mutable.HashMap(
        ("three", "0") -> Set("four"),
        ("four", "1") -> Set("four")
      )

    new NFA[State](states,
                   accepting,
                   startState,
                   alphabet,
                   transitionTable,
                   "ε")
  }

  test("Test merge NFAs") {
    val mergedNFA = Utility.merge(Set[NFA[State]](genNFA1(), genNFA2()), "ε")

    assert(
      mergedNFA.states
        .diff(
          Set("oneMERGED_POSTFIX ()",
              "twoMERGED_POSTFIX ()",
              "threeMERGED_POSTFIX ()",
              "fourMERGED_POSTFIX ()"))
        .isEmpty)

    assert(
      mergedNFA.acceptingStates
        .diff(Set("twoMERGED_POSTFIX ()", "fourMERGED_POSTFIX ()"))
        .isEmpty)

    assert(
      mergedNFA.startStates
        .diff(Set("oneMERGED_POSTFIX ()", "threeMERGED_POSTFIX ()"))
        .isEmpty)
      // TODO determine how to test transitions
  }
}
