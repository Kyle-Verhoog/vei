package compiler

import compiler.scanner.TokenDefinition

object Compiler {
  type State = String

  def main(args: Array[String]) {
    val dfa = Utility
      .merge(Set(
               TokenDefinition.IF(),
               TokenDefinition.ELSE(),
               TokenDefinition.INT()
             ),
             "ε")
      .createDfa()

    val input = "0 123 if else"
    runDfa(input, dfa)

    if (args.length.equals(0)) {
      return
    }
  }

  def runDfa(input: String, dfa: DFA[State]) = {
    var currentDfa = dfa

    val it = input.toIterator.buffered
    while (it.hasNext) {
      val currentChar = it.next().toString
      println("processing " + currentChar)
      if (!currentChar.equals(" ")) {
        currentDfa = currentDfa.next(currentChar)

        // Stop if complete, and cant proceed
        if (currentDfa.isComplete() && !currentDfa.canProceed(it.head.toString)) {
          currentDfa = dfa
          println("done")
        }
      }
    }
  }
}
