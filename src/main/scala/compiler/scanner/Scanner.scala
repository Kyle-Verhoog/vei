/**
  * Scanner which accepts a configuration of tokens given in the following
  * schema:
  *
  * tokens.lex:
  * <TOKEN1> "regular expression1"
  * <TOKEN2> "regular expression2"
  * ...
  * <TOKENN> "regular expressionN"
  */
package compiler.scanner

import regex.Regex

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream,
}
import java.util.Base64
import scala.collection.mutable

final case class ScanException(
    private val message: String = "Scanning failed somewhere in the source.",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object TokenEngine {
  def fromRegex(regex: String,
                token: Token,
                alphabet: Set[String]): TokenEngine = {
    val nfa = Regex.toNFA(regex, alphabet)
    // add the token states
    for (as <- nfa.acceptingStates) {
      nfa.tokenStates += (as -> token)
    }
    new TokenEngine(nfa)
  }

  def newEmptyEngine(alphabet: Set[String]): TokenEngine = {
    new TokenEngine(
      NFA.newStringNFA(
        states = Set[NFA.T](),
        acceptingStates = Set[NFA.T](),
        startStates = Set[NFA.T](),
        alphabet = alphabet,
        transitionTable = new mutable.HashMap[(NFA.T, String), Set[NFA.T]]()
      )
    )
  }
}

class TokenEngine(val nfa: NFA[String]) {
  def addRegex(regex: String, token: Token): TokenEngine = {
    val nfa2 = TokenEngine.fromRegex(regex, token, nfa.alphabet).nfa

    val transitionTable = Regex.mergeMaps(
      nfa.transitionTable,
      nfa2.transitionTable
    )

    val s = NFA.newState()
    val states = Set(s) | nfa.states | nfa2.states
    val acceptingStates = nfa.acceptingStates | nfa2.acceptingStates
    val startStates = Set(s)
    val tokenStates = Regex.mergeMaps(nfa.tokenStates, nfa2.tokenStates)

    var newNfa = new NFA(
      states,
      acceptingStates,
      startStates,
      nfa.alphabet,
      transitionTable,
      tokenStates,
      nfa.epsilonSym
    )
    newNfa = newNfa.addTransitions((s, "ε"), nfa.startStates | nfa2.startStates)

    new TokenEngine(newNfa)
  }
}

object Scanner {

  /**
    * Generates a new Scanner from a .lex configuration file.
    */
  def fromConfig(
      cfg: String,
      defaultAlphabet: Set[String] = Regex.asciiAlphabet): Scanner = {
    var rawTokens: String = ""
    var alphabet: Set[String] = Set()

    if (!cfg.contains("§")) {
      alphabet = defaultAlphabet
      rawTokens = cfg
    } else {
      val alphaConfig = cfg.split("§")(0)
      rawTokens = cfg.split("§")(1)
      alphabet = alphaConfig.toList.map(s => s.toString).toSet
    }
    val tokenConfig = rawTokens.trim.split("\n")
    var engine = TokenEngine.newEmptyEngine(alphabet)

    var tokenNumber = 0
    for (l <- tokenConfig.map(_.trim)) {
      val rawConf = l.split(" ")
      val token = new Token(rawConf(0), "", tokenNumber) // TODO fix this
      val regex = rawConf(1).substring(1, rawConf(1).length - 1)
      engine = engine.addRegex(regex, token)
      tokenNumber += 1
    }
    new Scanner(engine.nfa.toDFA())
  }

  /**
    * Generates a Scanner which has been previously serialized to a file.
    */
  def fromSerializedDFA(serializedDFA: String): Scanner = {
    val bytes = Base64.getDecoder.decode(serializedDFA)
    val bi = new ByteArrayInputStream(bytes)
    val si = new ObjectInputStream(bi) {
      // https://stackoverflow.com/a/22375260 !!!????
      override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
        try {
          Class.forName(desc.getName, false, getClass.getClassLoader)
        }
        catch {
          case ex: ClassNotFoundException => super.resolveClass(desc)
        }
      }
    }

    val dfa = si.readObject().asInstanceOf[DFA[String]]

    new Scanner(dfa)
  }

  def serializeDFA(dfa: DFA[String], filePath: String): String = {
    val bo = new ByteArrayOutputStream()
    val so = new ObjectOutputStream(bo)
    so.writeObject(dfa)
    so.flush()
    so.close()

    Base64.getEncoder.encodeToString(bo.toByteArray)
  }
}

class Scanner(val dfa: DFA[String]) {
  def scan(src: String,
           fileName: Option[String] = None): mutable.ListBuffer[Token] = {
    var tokens = mutable.ListBuffer[Token]()
    var curDFA = dfa
    var curTokenVal = ""
    var lastDFA = curDFA
    var lastTokenVal = curTokenVal
    var lastTokenEnd = 0
    var isComplete = false

    var i = 0
    /* Maximal-munch scanning.
     * Loop over the src string with a DFA. For each character that results in a
     * final DFA state, update `lastDFA`, `i` and `lastTokenVal`.
     * Then continue forward updating if necessary. If an exception is raised
     * (token scanned is not in the DFA) then use `lastDFA` to get the token.
     */
    while (i < src.length) {
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
          if (i == src.length - 1)
            throw new Exception()
        }
        i += 1
      } catch {
        case e: Throwable =>
          if (isComplete) {
            val token = lastDFA.getCurrentToken()
            tokens += new Token(
              token.tokenType,
              lastTokenVal,
              token.tokenNumber,
            )
            curDFA = dfa
            curTokenVal = ""
            // move back to just after the matched text
            i = lastTokenEnd + 1
          } else {
            val progress = src.substring(0, i)
            var indent = 0
            var lineNum = 0
            var x = i
            while (x > 0 && src.charAt(x) != '\n') {
              indent += 1
              x -= 1
            }
            x = i
            while (x > 0) {
              if (src.charAt(x) == '\n')
                lineNum += 1
              x -= 1
            }

            val pointer = (0 until indent - 1).toList
              .map(_ => "-")
              .mkString + "^"

            throw ScanException(
              s"""$progress
                 |$pointer
                 |Scan failed on char '$c' on line ${lineNum + 1} col $indent.
                 |
                 |Parsed '$curTokenVal' for current token.
                 |
                 |Additional error information:\n
                 |  $e\n\n""".stripMargin
            )
          }
          isComplete = false
      }
    }

    if (dfa != curDFA) {
      curDFA
        .getCurrentToken() // TODO HACK this will make it throw if not finish, since no tokens will exist
    }
    tokens
  }
}
