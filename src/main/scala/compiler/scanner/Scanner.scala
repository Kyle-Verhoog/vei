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
import java.io._
import java.util.Base64

import compiler.{DFA, NFA}
import regex.Regex

import scala.collection.mutable
import scala.io.Source

final case class ScanException(
    private val message: String = "Scanning failed somewhere in the source.",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object TokenEngine {
  def fromRegex(regex: String, token: Token): TokenEngine = {
    val nfa = Regex.toNFA(regex)
    // add the token states
    for (as <- nfa.acceptingStates) {
      nfa.tokenStates += (as -> token)
    }
    new TokenEngine(nfa)
  }

  def newEmptyEngine(): TokenEngine = {
    new TokenEngine(
      NFA.newStringNFA(
        states = Set[NFA.T](),
        acceptingStates = Set[NFA.T](),
        startStates = Set[NFA.T](),
        transitionTable = new mutable.HashMap[(NFA.T, String), Set[NFA.T]]()
      )
    )
  }
}

class TokenEngine(val nfa: NFA[String]) {
  def addRegex(regex: String, token: Token): TokenEngine = {
    val nfa2 = TokenEngine.fromRegex(regex, token).nfa

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
    var engine = TokenEngine.newEmptyEngine()

    var tokenNumber = 0
    for (l <- s.trim.split("\n").map(_.trim)) {
      val rawConf = l.split(" ")
      val token = new Token(rawConf(0), "", tokenNumber) // TODO fix this
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

  def scan(src: String): mutable.ListBuffer[Token] = {
    var tokens = mutable.ListBuffer[Token]()
    var curDFA = dfa
    var curTokenVal = ""
    var lastDFA = curDFA
    var lastTokenVal = curTokenVal
    var lastTokenEnd = 0
    var isComplete = false

    var i = 0
    /* Inadvertent maximal-munch scanning.
     * Loop over the src string with a DFA. For each character that results in a
     * final DFA state, update `lastDFA`, `i` and `lastTokenVal`.
     * Then continue forward updating if necessary. If an exception is raised
     * (token scanned is not in the DFA) then use `lastDFA` to get the token.
     */
    while (i < src.length()) {
      var c = src.charAt(i)

      // println(s"""
      //      | $i $c $curTokenVal
      //    """.stripMargin)
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
