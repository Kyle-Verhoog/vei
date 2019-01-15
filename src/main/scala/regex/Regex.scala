package regex
import scala.collection.{SortedSet, mutable}
import compiler.scanner.Token
import scala.collection.mutable.{HashMap, ListBuffer}
import compiler.Compiler.State
import compiler.NFA

case class Paren(val nalt: Integer, val natom: Integer) {}

class RegexEngine(val expr: String, val nfa: NFA[String]) {
  def matches(s: String): Boolean = {
    var n = nfa
    try {
      for (c <- s) {
        n = n.next(c.toString)
      }
    } catch {
      // TODO: we could be more descriptive here.
      case _: Throwable => return false
    }

    return n.isComplete()
  }
}

object Regex {
  val ALT = "∪"
  val CONCAT = "·"
  val LPAREN = "⦅"
  val RPAREN = "⦆"
  val LBRACK = "〚"
  val RBRACK = "〛"
  val OOM = "⨁"
  val ZOM = "⨂"
  val ZOO = "⁇"

  def preProcess(regex: String): String = {
    val temp = regex
      .replaceAllLiterally("[", LBRACK)
      .replaceAllLiterally(s"\\$LBRACK", "[")
      .replaceAllLiterally("]", RBRACK)
      .replaceAllLiterally(s"\\$RBRACK", "]")

    var processedRegex = ""

    var i = 0
    while (i < temp.length) {
      if (temp.charAt(i).toString.equals(LBRACK)) {
        val range =
          (temp.charAt(i + 1) to temp.charAt(i + 3)).map(ch => ch.toString)
        processedRegex += "(" + range.mkString("|") + ")"
        i += 4
      } else {
        processedRegex += temp.charAt(i)
      }
      i += 1
    }

    processedRegex = processedRegex
      .replaceAllLiterally("(", LPAREN)
      .replaceAllLiterally(s"\\$LPAREN", "(")
      .replaceAllLiterally(")", RPAREN)
      .replaceAllLiterally(s"\\$RPAREN", ")")
      .replaceAllLiterally("|", ALT)
      .replaceAllLiterally(s"\\$ALT", "|")
      .replaceAllLiterally("+", OOM)
      .replaceAllLiterally(s"\\$OOM", "+")
      .replaceAllLiterally("*", ZOM)
      .replaceAllLiterally(s"\\$ZOM", "*")
      .replaceAllLiterally("?", ZOO)
      .replaceAllLiterally(s"\\$ZOO", "?")

    //println(processedRegex)
    processedRegex
  }

  def toPostfix(regex: String): String = {
    // TODO figure out if/how we want to handle escaping special chars
    val processedRegex = preProcess(regex)
    // println("Replaced regex " + regex + "     with     " + processedRegex)

    var postfix = ""
    var x = 0
    var natom = 0
    var nalt = 0
    var parens = new ListBuffer[Paren]()

    for (x <- 0 until processedRegex.length()) {
      val re = processedRegex.charAt(x).toString

      re match {
        case LPAREN => {
          if (natom > 1) {
            natom -= 1
            postfix += CONCAT
          }

          parens += new Paren(nalt, natom)
          nalt = 0
          natom = 0
        }
        case ALT => {
          if (natom == 0) {
            println("Error") // TODO: throw
          }
          natom -= 1
          while (natom > 0) {
            postfix += CONCAT
            natom -= 1
          }
          nalt += 1
        }
        case RPAREN => {
          if (parens.length < 1) {
            println("error 1") // TODO: throw
          }
          if (natom == 0) {
            println("error 2") // TODO: throw
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

          var par = parens.remove(parens.length - 1)
          nalt = par.nalt
          natom = par.natom
          natom += 1
        }
        case ZOO | ZOM | OOM => {
          if (natom == 0) {
            println("Error symb") // throws
          }
          postfix += re
        }
        case _ => {
          if (natom > 1) {
            natom -= 1
            postfix += CONCAT
          }

          postfix += re
          natom += 1
        }
      }
    }

    if (parens.length > 0) {
      // TODO throw
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

  def mergeMaps[T, U](m1: HashMap[T, U], m2: HashMap[T, U]): HashMap[T, U] = {
    var map = new HashMap[T, U]()

    m1 foreach {
      case (key, value) => {
        map += (key -> value)
      }
    }
    m2 foreach {
      case (key, value) => {
        map += (key -> value)
      }
    }
    map
  }

  def newNFA(
      states: Set[State],
      acceptingStates: Set[State],
      startStates: Set[State],
      transitionTable: mutable.HashMap[(State, String), Set[State]]
  ): NFA[String] = {
    val alpha = Set("a", "b", "c")
    new NFA[String](
      states,
      acceptingStates,
      startStates,
      alpha,
      transitionTable,
      HashMap[State, Token](),
      "ε"
    )
  }

  def newState(): State = {
    scala.util.Random.alphanumeric.take(16).mkString
  }

  def postfixToNFA(postfix: String): NFA[String] = {
    var stack = new ListBuffer[NFA[String]]()
    var x = 0

    for (x <- 0 until postfix.length()) {
      val p = postfix.charAt(x).toString

      p match {
        case ZOM => {
          val e = stack.remove(stack.length - 1)

          val s = newState()
          val states = Set(s) | e.states
          val startStates = Set(s)
          val acceptingStates = Set(s) | e.acceptingStates

          var nfa = newNFA(
            states,
            acceptingStates,
            startStates,
            e.transitionTable
          )

          nfa = nfa.addTransition((s, "ε"), e.startStates)
          for (as <- e.acceptingStates) {
            nfa = nfa.addTransition((as, "ε"), Set(s))
          }
          stack += nfa
        }
        case ALT => {
          val e2 = stack.remove(stack.length - 1)
          val e1 = stack.remove(stack.length - 1)

          var transitionTable = mergeMaps(
            e1.transitionTable,
            e2.transitionTable
          )

          // Add new state to connect e1, e2
          val s = newState()
          var states = Set(s) | e1.states | e2.states

          // Accepting states are the accepting states of e1 OR e2
          var acceptingStates = e1.acceptingStates | e2.acceptingStates

          // s is the new start state
          var startStates = Set(s)

          var nfa = newNFA(
            states,
            acceptingStates,
            startStates,
            transitionTable
          )
          // Add the transitions from s to the start states of e1, e2
          nfa = nfa.addTransition((s, "ε"), e1.startStates | e2.startStates)

          stack += nfa
        }
        case CONCAT => {
          val e2 = stack.remove(stack.length - 1)
          val e1 = stack.remove(stack.length - 1)

          var transitionTable = mergeMaps(
            e1.transitionTable,
            e2.transitionTable
          )

          for (a <- e1.acceptingStates) {
            transitionTable += ((a, "ε") -> e2.startStates) // TODO: copy e2.startStates?
          }

          var newStates = e1.states | e2.states
          var newAcceptingStates = e2.acceptingStates
          var newStartStates = e1.startStates

          var nfa = newNFA(
            newStates,
            newAcceptingStates,
            newStartStates,
            transitionTable
          )
          stack += nfa
        }
        case _ => {
          val _ps = newState()
          val ps = newState()

          var nfa = newNFA(
            Set[State](_ps, ps),
            Set[State](ps),
            Set[State](_ps),
            HashMap((_ps, p) -> Set(ps))
          )

          stack += nfa
        }
      }
    }
    stack(0)
  }

  def toNFA(regex: String): NFA[String] = {
    val postfix = toPostfix(regex)
    val nfa = postfixToNFA(postfix)
    nfa
  }

  def createEngine(expr: String, token: String = ""): RegexEngine = {
    new RegexEngine(expr, toNFA(expr))
  }
}
