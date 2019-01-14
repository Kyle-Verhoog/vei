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
      val regex = rawConf(1).substring(1, rawConf(1).length-1)
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
    var d = dfa
    var lastDFA = d
    var i = 0
    var isComplete = false
    var v = ""
    var lastv = v

    var x = 0
    while (x < src.length()) {
      var s = src.charAt(x)
      try {
        d = d.next(s.toString)
        v += s
        if (d.isComplete()) {
          lastv = v
          i = x
          isComplete = true
          lastDFA = d
          // hack: to handle when a DFA is matched on the last character
          if (x == src.length()-1)
            throw new Exception()
        }
        x += 1
      }
      catch {
        case _: Throwable => {
          if (isComplete) {
            var token = lastDFA.getCurrentToken()
            var ttype = token.tokenType
            token.value = lastv
            tokens += token
            println(s"TOKEN: $ttype ($lastv)")
            d = dfa
            v = ""
            // move back to just after the matched text
            x = i + 1
          }
          else {
            println(s"LEX ERROR on char '$s', parsed '$v' at position $x")
            x = src.length()
          }
          isComplete = false
        }
      }
    }
    tokens
  }
}
