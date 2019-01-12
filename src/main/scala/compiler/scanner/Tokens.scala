package compiler.scanner

import compiler.Compiler.State
import compiler.NFA

import scala.collection.mutable

// TODO figure out how we want to do this
object Tokens {
  def genDigitRange(lower: Int, upper: Int): Set[String] = {
    (lower to upper).map(digit => digit.toString).toSet
  }

  def genAlphaRange(lower: Char, upper: Char): Set[String] = {
    (lower to upper).map(char => char.toString).toSet
  }

  def INT(): NFA[State] = {
    val states = Set("start", "zero", "digit")
    val accepting = Set("zero", "digit")
    val startState = Set("start")
    val alphabet = genDigitRange(0, 9)
    val transitionTable: mutable.HashMap[(State, State), Set[State]] =
      mutable.HashMap(("start", "0") -> Set("zero"))

    // add transitions for 1-9 to start an int
    genDigitRange(1, 9).foreach(digit =>
      transitionTable += ("start", digit) -> Set("digit"))

    // add transitions for 0-9 to continue an int
    genDigitRange(1, 9).foreach(digit =>
      transitionTable += ("digit", digit) -> Set("digit"))

    new NFA[State](states,
                   accepting,
                   startState,
                   alphabet,
                   transitionTable,
                   "ε")
  }

  // TODO: The NFAs below we can probably do something else with since they are just words, but I wanted to test with them
  def IF(): NFA[State] = {
    val states = Set("start", "i", "f")
    val accepting = Set("f")
    val startState = Set("start")
    val alphabet = Set("i", "f")
    val transitionTable: mutable.HashMap[(State, State), Set[State]] =
      mutable.HashMap(("start", "i") -> Set("i"), ("i", "f") -> Set("f"))

    new NFA[State](states,
                   accepting,
                   startState,
                   alphabet,
                   transitionTable,
                   "ε")
  }

  def ELSE(): NFA[State] = {
    val states = Set("start", "e", "l", "s", "e2")
    val accepting = Set("e2")
    val startState = Set("start")
    val alphabet = Set("e", "l", "s")
    val transitionTable: mutable.HashMap[(State, State), Set[State]] =
      mutable.HashMap(
        ("start", "e") -> Set("e"),
        ("e", "l") -> Set("l"),
        ("l", "s") -> Set("s"),
        ("s", "e") -> Set("e2")
      )

    new NFA[State](states,
                   accepting,
                   startState,
                   alphabet,
                   transitionTable,
                   "ε")
  }
}
