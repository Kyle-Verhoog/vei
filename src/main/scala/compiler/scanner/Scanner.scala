package compiler.scanner
import compiler.{DFA, NFA}
import regex.Regex


object TokenEngine {
  def fromRegex(regex: String, token: Token): TokenEngine = {
    val nfa = Regex.toNFA(regex)
    // add the token states
    for (as <- nfa.acceptingStates) {
      nfa.tokenStates += (as -> token)
    }
    new TokenEngine(nfa)
  }
}

class TokenEngine(val nfa: NFA[String]) {
  def addRegex(regex: String, token: Token): TokenEngine = {
    val nfa2 = TokenEngine.fromRegex(regex, token).nfa

    var transitionTable = Regex.mergeMaps(
      nfa.transitionTable,
      nfa2.transitionTable
    )

    val s = Regex.newState()
    var states = Set(s) | nfa.states | nfa2.states
    var acceptingStates = nfa.acceptingStates | nfa2.acceptingStates
    var startStates = Set(s)
    var tokenStates = Regex.mergeMaps(nfa.tokenStates, nfa2.tokenStates)

    var newNfa = new NFA(
      states,
      acceptingStates,
      startStates,
      Set("i", "f", "a", "b", "c", "d", "1", "2", "=", ";", "[", "]", "{", "}", " "),
      transitionTable,
      tokenStates,
      nfa.epsilonSym,
    )
    newNfa = newNfa.addTransition((s, "ε"), nfa.startStates | nfa2.startStates)

    new TokenEngine(newNfa)
  }
}

object Scanner {
  def fromConfig(s: String): Scanner = {
    var engine = TokenEngine.fromRegex(" ", new Token("Whitespace", " "))
    for (l <- s.split("\n").map(_.trim)) {
      val rawConf = l.split(" ")
      val token = new Token(rawConf(0), "some value") // TODO fix this
      val regex = rawConf(1).substring(1, rawConf(1).length-1)
      engine = engine.addRegex(regex, token)
    }
    println(engine.nfa)
    new Scanner(engine.nfa.createDfa())
  }
}

class Scanner(val dfa: DFA[String]) {
  def scan(src: String) {
    println(dfa)
    var d = dfa
    var v = ""
    for (s <- src) {
      //try {
        d = d.next(s.toString)
        v += s
        if (d.isComplete()) {
          var token = d.getCurrentToken()
          println(s"TOKEN: $token ($v) ")
          v = ""
          d = dfa // reset dfa
        }
      // }
      // catch {
      //   case _: Throwable => println("LEX ERROR")
      // }
    }
  }
}
