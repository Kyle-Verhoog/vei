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
      (0 to 127).map(i => i.toChar.toString).toSet, // all ASCII chars
      // (' ' to '~').map(ch=> ch.toString).toSet,
      transitionTable,
      tokenStates,
      nfa.epsilonSym
    )
    newNfa = newNfa.addTransition((s, "ε"), nfa.startStates | nfa2.startStates)

    new TokenEngine(newNfa)
  }
}

object Scanner {
  def fromConfig(s: String): Scanner = {
    // TODO: support spaces/special chars in config
    var engine = TokenEngine.fromRegex("\n", new Token("NEWLINE", " "))
    engine = engine.addRegex(" ", new Token("WHITESPACE", " "))
    for (l <- s.split("\n").map(_.trim)) {
      val rawConf = l.split(" ")
      val token = new Token(rawConf(0), "some value") // TODO fix this
      val regex = rawConf(1).substring(1, rawConf(1).length-1)
      engine = engine.addRegex(regex, token)
    }
    // println(engine.nfa)
    new Scanner(engine.nfa.createDfa())
  }
}

class Scanner(val dfa: DFA[String]) {
  def scan(src: String) {
    // println(dfa)

    // TODO: HACK
    // NFA -> DFA accepting states in NFA should be in DFA
    // it seems like it's possible that accepting states don't have entries added to the tokenStates map
    dfa.tokenStates foreach {
      case (stok, token) => {
        dfa.transitionTable foreach {
          case ((stra, ch), out) => {
            if (stra contains stok) {
              dfa.tokenStates += (stra -> token)
            }
          }
        }
      }
    }

    // println(dfa)
    var d = dfa
    var v = ""
    for (s <- src) {
      // try {
        d = d.next(s.toString)
        v += s
        if (d.isComplete()) {
          var token = d.getCurrentToken()
          var ttype = token.tokenType
          println(s"TOKEN: $ttype ($v)")
          v = ""
          d = dfa // reset dfa
        }
        else {
        }
      // }
      // catch {
      //   case _: Throwable => {
      //     println("LEX ERROR")
      //   }
      // }
    }
  }
}
