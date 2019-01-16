package compiler.scanner
import java.io._
import java.util.Base64

import compiler.{DFA, NFA}
import regex.Regex

import scala.collection.mutable.ListBuffer
import scala.io.Source

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

    val s = NFA.newState()
    var states = Set(s) | nfa.states | nfa2.states
    var acceptingStates = nfa.acceptingStates | nfa2.acceptingStates
    var startStates = Set(s)
    var tokenStates = Regex.mergeMaps(nfa.tokenStates, nfa2.tokenStates)

    var newNfa = new NFA(
      states,
      acceptingStates,
      startStates,
      (0 to 127).map(i => i.toChar.toString).toSet, // all ASCII chars
      transitionTable,
      tokenStates,
      nfa.epsilonSym
    )
    newNfa = newNfa.addTransitions((s, "ε"), nfa.startStates | nfa2.startStates)

    new TokenEngine(newNfa)
  }
}

object Scanner {
  def fromConfig(s: String): Scanner = {
    // TODO: support spaces/special chars in config
    var engine = TokenEngine.fromRegex("\n", new Token("NEWLINE", " "))
    engine = engine.addRegex(" ", new Token("WHITESPACE", " "))

    var tokenNumber = 0
    for (l <- s.split("\n").map(_.trim)) {
      val rawConf = l.split(" ")
      val token = new Token(rawConf(0), "some value", tokenNumber) // TODO fix this
      val regex = rawConf(1).substring(1, rawConf(1).length - 1)
      engine = engine.addRegex(regex, token)
      tokenNumber += 1
    }
    new Scanner(engine.nfa.toDFA())
  }

  def serializeDfa(dfa: DFA[String], filePath: String): Unit = {
    val bo = new ByteArrayOutputStream()
    val so = new ObjectOutputStream(bo)
    so.writeObject(dfa)
    so.flush()
    so.close()

    val file = new File(filePath)
    println("writing to " + filePath)
    val pw = new PrintWriter(file)
    pw.write(Base64.getEncoder.encodeToString(bo.toByteArray))
    pw.close()
  }

  def deserializeDfa(): DFA[String] = {
    val fileContents =
      Source.fromResource("serializations/dfa_serialization").mkString
    val bytes = Base64.getDecoder.decode(fileContents)
    val bi = new ByteArrayInputStream(bytes)
    val si = new ObjectInputStream(bi)

    si.readObject().asInstanceOf[DFA[String]]
  }

  def deserializeDfa(filePath: String): DFA[String] = {
    val fileContents = Source.fromFile(filePath).mkString
    val bytes = Base64.getDecoder.decode(fileContents)
    val bi = new ByteArrayInputStream(bytes)
    val si = new ObjectInputStream(bi)

    si.readObject().asInstanceOf[DFA[String]]
  }
}

class Scanner(val dfa: DFA[String], val fileName: String) {
  // constructor if only given dfa.txt
  def this(dfa: DFA[String]) {
    this(dfa, "")
  }

  // constructor if only given file to deserialize
  def this(fileToDeserializeDfa: String) {
    this(Scanner.deserializeDfa(fileToDeserializeDfa))
  }

  // constructor if given nothing (use default serialization place
  def this() {
    this(Scanner.deserializeDfa())
  }

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
            tokens += new Token(
              ttype,
              lastTokenVal,
              lastDFA.getCurrentToken().tokenNumber
            )
            curDFA = dfa
            curTokenVal = ""
            // move back to just after the matched text
            i = lastTokenEnd + 1
          } else {
            // TODO: throw new LexException
            println(
              s"LEX ERROR on char '$c', parsed '$curTokenVal' at position $i"
            )
            i = src.length() // break out of loop
          }
          isComplete = false
        }
      }
    }
    tokens
  }
}
