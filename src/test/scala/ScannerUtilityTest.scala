/*
import compiler.Compiler.State
import compiler.Utility
import compiler.NFA
import compiler.scanner.{Token, TokenDefinition}
import exceptions.{TransitionNonExistentException}
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
    val tokenStates: mutable.HashMap[State, Token] = mutable.HashMap()

    new NFA[State](
      states,
      accepting,
      startState,
      alphabet,
      transitionTable,
      tokenStates,
      "ε"
    )
  }

  def genNFA2(): NFA[State] = {
    val states = Set("three", "four", "five")
    val accepting = Set("four", "five")
    val startState = Set("three", "five")
    val alphabet = Set("0", "1")
    val transitionTable: mutable.HashMap[(State, State), Set[State]] =
      mutable.HashMap(
        ("three", "0") -> Set("four"),
        ("three", "0") -> Set("five"),
        ("three", "1") -> Set("five"),
        ("four", "1") -> Set("four")
      )
    val tokenStates: mutable.HashMap[State, Token] = mutable.HashMap()

    new NFA[State](
      states,
      accepting,
      startState,
      alphabet,
      transitionTable,
      tokenStates,
      "ε"
    )
  }

  test("Test merge NFAs") {
    val mergedNFA = Utility.merge(Set[NFA[State]](genNFA1(), genNFA2()), "ε")

    assert(
      mergedNFA.states
        .diff(
          Set(
            "oneMERGED_POSTFIX1",
            "twoMERGED_POSTFIX1",
            "threeMERGED_POSTFIX2",
            "fourMERGED_POSTFIX2",
            "fiveMERGED_POSTFIX2"
          )
        )
        .isEmpty
    )

    assert(
      mergedNFA.acceptingStates
        .diff(
          Set(
            "twoMERGED_POSTFIX1",
            "fourMERGED_POSTFIX2",
            "fiveMERGED_POSTFIX2"
          )
        )
        .isEmpty
    )

    assert(
      mergedNFA.startStates
        .diff(
          Set(
            "oneMERGED_POSTFIX1",
            "threeMERGED_POSTFIX2",
            "fiveMERGED_POSTFIX2"
          )
        )
        .isEmpty
    )

    // TODO how many transitions we want to test
  }

  test("Test runDfa on valid tokens") {
    val dfa = Utility
      .merge(
        Set(
          TokenDefinition.IF(),
          TokenDefinition.ELSE(),
          TokenDefinition.INT()
        ),
        "ε"
      )
      .toDFA()

    val input = "0    123 if else 78       234"
    val tokens = Utility.runDfa(input, dfa)
    assert(
      tokens.equals(
        mutable.MutableList(
          new Token("INTEGER", "val"),
          new Token("INTEGER", "val"),
          new Token("IF", "val"),
          new Token("ELSE", "val"),
          new Token("INTEGER", "val"),
          new Token("INTEGER", "val")
        )
      )
    )
  }

  test("Test runDfa on some invalid tokens") {
    val dfa = Utility
      .merge(
        Set(
          TokenDefinition.IF(),
          TokenDefinition.ELSE(),
          TokenDefinition.INT()
        ),
        "ε"
      )
      .toDFA()

    val input = "0 123 bad if else"
    assertThrows[TransitionNonExistentException] {
      Utility.runDfa(input, dfa)
    }
  }
}
*/
