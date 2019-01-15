package compiler.scanner
import compiler.{DFA, NFA}
import regex.Regex
import scala.collection.mutable.{ListBuffer}

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
      val regex = rawConf(1).substring(1, rawConf(1).length - 1)
      engine = engine.addRegex(regex, token)
    }
    new Scanner(engine.nfa.createDfa())
  }
}

class Scanner(val dfa: DFA[String]) {
  def scan(src: String): ListBuffer[Token] = {
    var tokens = ListBuffer[Token]()
    // TODO: HACK
    // NFA -> DFA accepting states in NFA should be in DFA
    // it seems like it's possible that accepting states don't have entries added to the tokenStates map
    var curDFA = dfa
    var curTokenVal = ""
    var lastDFA = curDFA
    var lastTokenVal = curTokenVal
    var lastTokenEnd = 0
    var isComplete = false

    var i = 0
    /* Inadvertant maximal-munch scanning.
     * Loop over the src string with a DFA. For each character that
     * results in a final DFA state, update `lastDFA`, `i` and `lastv`.
     * Then continue forward updating if necessary. If an exception
     * is raised (token scanned is not in the DFA) then use `lastDFA`
     * to get the token.
     */
    while (i < src.length()) {
      var c = src.charAt(i)

      try {
        curDFA = curDFA.next(c.toString)
        curTokenVal += c
        if (curDFA.isComplete()) {
          lastTokenVal = curTokenVal
          lastTokenEnd = i
          isComplete = true
          lastDFA = curDFA
          // hack: to handle when a DFA is matched on the last character
          if (i == src.length() - 1)
            throw new Exception()
        }
        i += 1
      } catch {
        case _: Throwable => {
          if (isComplete) {
            var token = lastDFA.getCurrentToken()
            var ttype = token.tokenType
            // token.value = lastTokenVal
            tokens += new Token(ttype, lastTokenVal)
            println(s"TOKEN: $ttype ($lastTokenVal)")
            curDFA = dfa
            curTokenVal = ""
            // move back to just after the matched text
            i = lastTokenEnd + 1
          } else {
            // TODO: throw new LexException
            println(s"LEX ERROR on char '$c', parsed '$curTokenVal' at position $i")
            i = src.length() // break out of loop
          }
          isComplete = false
        }
      }
    }
    tokens
  }
}
