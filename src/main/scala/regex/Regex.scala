/**
  * A simplified regular expression implementation. Provides functionality for
  * matching or generating NFAs.
  *
  * The following operators are supported:
  *  - () (precedence)
  *  - | (alternation)
  *  - * (zero-or-more)
  *  - [x-y] (range of characters from x to y)
  *  - . (match-any)
  *
  *  To escape the above characters prefix them \
  */
package regex
import scala.collection.mutable
import compiler.{DFA, NFA}

case class Paren(val nalt: Integer, val natom: Integer) {}

final case class RegexParseException(
    private val message: String = "An error occurred while parsing regex",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

final case class RegexNFAConversionException(
    private val message: String =
      "An error occurred while converting regex to NFA",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

class RegexEngine(val expr: String, val dfa: DFA[String]) {
  def matches(s: String): Boolean = {
    var d = dfa
    try {
      for (c <- s) {
        d = d.next(c.toString)
      }
    } catch {
      // TODO: we could be more descriptive here.
      case _: Throwable => return false
    }

    d.isComplete()
  }
}

object Regex {
  val ALT = "∪"
  val CONCAT = "·"
  val ZOM = "⨂"
  val LPAREN = "⦅"
  val RPAREN = "⦆"
  val LBRACK = "〚"
  val RBRACK = "〛"
  val OOM = "⨁"
  val ZOO = "⁇"
  val NEW_LINE = "☭"
  val SPACE = "☃"
  val TAB = "☘"
  val DOT = "ø"
  val NOT = "¬"

  val escapeMapping = Map(
    ALT -> "|",
    "|" -> ALT,
    ZOM -> "*",
    "*" -> ZOM,
    LPAREN -> "(",
    "(" -> LPAREN,
    RPAREN -> ")",
    ")" -> RPAREN,
    LBRACK -> "[",
    "[" -> LBRACK,
    RBRACK -> "]",
    "]" -> RBRACK,
  )

  def expandRanges(regex: String): String = {
    var processedRegex = ""
    var i = 0
    while (i < regex.length) {
      if (regex.charAt(i).toString.equals(LBRACK)) {
        val range =
          (regex.charAt(i + 1) to regex.charAt(i + 3))
            .map(ch => ch.toString)
            .mkString(ALT)
        processedRegex += LPAREN + range + RPAREN
        i += 4
      } else {
        processedRegex += regex.charAt(i)
      }
      i += 1
    }

    processedRegex
  }

  def replaceDot(regex: String): String = {
    val allChars = (0 to 127).map(i => i.toChar.toString).mkString(ALT)
    regex.replaceAllLiterally(DOT, LPAREN + allChars + RPAREN)
  }

  def addUnicode(regex: String): String = {
    regex
      .replaceAllLiterally("(", LPAREN)
      .replaceAllLiterally(s"\\$LPAREN", "(")
      .replaceAllLiterally(")", RPAREN)
      .replaceAllLiterally(s"\\$RPAREN", ")")
      .replaceAllLiterally("[", LBRACK)
      .replaceAllLiterally(s"\\$LBRACK", "[")
      .replaceAllLiterally("]", RBRACK)
      .replaceAllLiterally(s"\\$RBRACK", "]")
      .replaceAllLiterally("|", ALT)
      .replaceAllLiterally(s"\\$ALT", "|")
      .replaceAllLiterally("+", OOM)
      .replaceAllLiterally(s"\\$OOM", "+")
      .replaceAllLiterally("*", ZOM)
      .replaceAllLiterally(s"\\$ZOM", "*")
      .replaceAllLiterally("?", ZOO)
      .replaceAllLiterally(s"\\$ZOO", "?")
      .replaceAllLiterally(".", DOT)
      .replaceAllLiterally(s"\\$DOT", ".")
      .replaceAllLiterally(s"$NEW_LINE", "\n")
      .replaceAllLiterally(s"$SPACE", " ")
      .replaceAllLiterally(s"$TAB", "\t")
  }

  def preProcess(regex: String): String = {
    val unicodeRegex = addUnicode(regex)
    val rangedRegex = expandRanges(unicodeRegex)
    //println(replaceDot(rangedRegex))
    replaceDot(rangedRegex)
  }

  def toPostfix(regex: String): String = {
    val processedRegex = preProcess(regex)

    if (processedRegex.isEmpty) {
      throw RegexParseException("regex cannot be empty!")
    }

    var postfix = ""
    var natom = 0
    var nalt = 0
    var parens = new mutable.ListBuffer[Paren]()

    for (x <- 0 until processedRegex.length()) {
      val re = processedRegex.charAt(x).toString

      re match {
        case LPAREN =>
          if (natom > 1) {
            natom -= 1
            postfix += CONCAT
          }

          parens += Paren(nalt, natom)
          nalt = 0
          natom = 0
        case ALT =>
          if (natom == 0) {
            throw new RuntimeException()
          }
          natom -= 1
          while (natom > 0) {
            postfix += CONCAT
            natom -= 1
          }
          nalt += 1
        case RPAREN =>
          if (parens.length < 1) {
            throw RegexParseException(
              s"'$regex': missing opening paren somewhere before position $x")
          }
          if (natom == 0) {
            throw RegexParseException(
              s"'$regex': missing atom before paren at position $x")
          }

          natom -= 1
          while (natom > 0) {
            postfix += CONCAT
            natom -= 1
          }

          while (nalt > 0) {
            postfix += ALT
            nalt -= 1
          }

          val par = parens.remove(parens.length - 1)
          nalt = par.nalt
          natom = par.natom
          natom += 1
        case o if o == ZOM || o == OOM || o == ZOO || o == NOT =>
          if (natom == 0) {
            throw RegexParseException(
              s"'$regex': missing operand for ${escapeMapping(o)} operator at position $x")
          }
          postfix += re
        case _ =>
          if (natom > 1) {
            natom -= 1
            postfix += CONCAT
          }

          postfix += re
          natom += 1
      }
    }

    if (parens.nonEmpty) {
      throw RegexParseException(
        s"'$regex': Unclosed paren at position ${regex.length}")
    }

    natom -= 1
    while (natom > 0) {
      postfix += CONCAT
      natom -= 1
    }

    while (nalt > 0) {
      postfix += ALT
      nalt -= 1
    }
    postfix
  }

  def mergeMaps[T, U](m1: mutable.HashMap[T, U],
                      m2: mutable.HashMap[T, U]): mutable.HashMap[T, U] = {
    var map = new mutable.HashMap[T, U]()

    m1 foreach {
      case (key, value) => map += (key -> value)
    }
    m2 foreach {
      case (key, value) => map += (key -> value)
    }
    map
  }

  def postfixToNFA(postfix: String): NFA[String] = {
    var stack = new mutable.ListBuffer[NFA[String]]()

    for (x <- 0 until postfix.length()) {
      val p = postfix.charAt(x).toString

      p match {
        case NOT =>
          /**
            * The NOT operator can only be applied to concatenated atoms.
            *
            * Visually:
            *          a      b      ...
            * (start) -> (1) -> (2) ----> (())
            *
            * Becomes
            *            Σ\{a}
            * (start) -----------> (())
                    |  a         b        ...
            *       +-----> (()) -> (()) -----> ()
            */
          val e = stack.remove(stack.length - 1)

          val startState = NFA.newState()
          var states = Set[NFA.T]()
          var acceptingStates = Set[NFA.T]()
          val startStates = Set(startState)
          val transitionTable =
            new mutable.HashMap[(NFA.T, String), Set[NFA.T]]()

          // Current state when looping through e, the old NFA
          var eCurState = e.startStates.head
          // Current state of the new NFA
          var newCurState = startState

          // TODO:
          // This loop can spin infinitely if there are circular transitions
          while (e.transitionTable.exists(t => t._1._1 == eCurState)) {
            val transitions = e.transitionTable.filter(t => {
              t._1._1 == eCurState
            })
            if (transitions.size != 1) {
              throw RegexParseException(
                s"$NOT operator can only be applied to concatenated atoms")
            }

            val transition = transitions.head
            val token = transition._1._2
            val eNextStates = transition._2

            if (eNextStates.size > 1) {
              throw RegexParseException(
                s"$NOT operator can only be applied to concatenated atoms")
            }

            val eNextState = eNextStates.head

            // Handle the start case matching all other characters
            if (eCurState == e.startStates.head) {
              val nextState = NFA.newState()
              acceptingStates = acceptingStates | Set(nextState)
              states = states | Set(nextState)
              for (c <- e.alphabet - token) {
                transitionTable += ((newCurState, c) -> Set(nextState))
              }
            } else {
              acceptingStates = acceptingStates | Set(newCurState)
            }

            val newNextState = NFA.newState()
            states = states | Set(newNextState)
            transitionTable += ((newCurState, token) -> Set(newNextState))
            eCurState = eNextState
            newCurState = newNextState
          }

          var nfa = NFA.newStringNFA(
            states = states,
            acceptingStates = acceptingStates,
            startStates = startStates,
            transitionTable = transitionTable,
          )

          stack += nfa
        case ZOM =>
          val e = stack.remove(stack.length - 1)

          val s = NFA.newState()
          val states = Set(s) | e.states
          val startStates = Set(s)
          val acceptingStates = Set(s) | e.acceptingStates

          var nfa = NFA.newStringNFA(
            states,
            acceptingStates,
            startStates,
            e.transitionTable
          )

          nfa = nfa.addTransitions((s, "ε"), e.startStates)
          for (as <- e.acceptingStates) {
            nfa = nfa.addTransitions((as, "ε"), Set(s))
          }
          stack += nfa
        case ZOO =>
          /**
            *   [e1]
            *          ε
            *   ((s)) -> [e1]
            */
          val e = stack.remove(stack.length - 1)

          val s = NFA.newState()
          val states = Set(s) | e.states
          val startStates = Set(s)
          val acceptingStates = Set(s) | e.acceptingStates

          var nfa = NFA.newStringNFA(
            states,
            acceptingStates,
            startStates,
            e.transitionTable
          )

          nfa = nfa.addTransitions((s, "ε"), e.startStates)
          stack += nfa
        case ALT =>
          val e2 = stack.remove(stack.length - 1)
          val e1 = stack.remove(stack.length - 1)

          val transitionTable = mergeMaps(
            e1.transitionTable,
            e2.transitionTable
          )

          // Add new state to connect e1, e2
          val s = NFA.newState()
          val states = Set(s) | e1.states | e2.states

          // Accepting states are the accepting states of e1 OR e2
          val acceptingStates = e1.acceptingStates | e2.acceptingStates

          // s is the new start state
          val startStates = Set(s)

          var nfa = NFA.newStringNFA(
            states,
            acceptingStates,
            startStates,
            transitionTable
          )
          // Add the transitions from s to the start states of e1, e2
          nfa = nfa.addTransitions((s, "ε"), e1.startStates | e2.startStates)

          stack += nfa
        case CONCAT =>
          val e2 = stack.remove(stack.length - 1)
          val e1 = stack.remove(stack.length - 1)

          val transitionTable = mergeMaps(
            e1.transitionTable,
            e2.transitionTable
          )

          val newStates = e1.states | e2.states
          val newAcceptingStates = e2.acceptingStates
          val newStartStates = e1.startStates

          var nfa = NFA.newStringNFA(
            newStates,
            newAcceptingStates,
            newStartStates,
            transitionTable
          )
          for (a <- e1.acceptingStates) {
            nfa.addTransitions((a, "ε"), e2.startStates)
          }

          stack += nfa
        case _ =>
          val _ps = NFA.newState()
          val ps = NFA.newState()

          var nfa = NFA.newStringNFA(
            Set[NFA.T](_ps, ps),
            Set[NFA.T](ps),
            Set[NFA.T](_ps),
            mutable.HashMap((_ps, p) -> Set(ps))
          )

          stack += nfa
      }
    }

    if (stack.length != 1) {
      throw RegexNFAConversionException(
        s"'$postfix': unexpected stack size for stack $stack")
    }
    stack.head
  }

  def toNFA(regex: String): NFA[String] = {
    val postfix = toPostfix(regex)
    val nfa = postfixToNFA(postfix)
    nfa
  }

  def createEngine(expr: String): RegexEngine = {
    val nfa = toNFA(expr)
    val dfa = nfa.toDFA()
    new RegexEngine(expr, dfa)
  }
}
