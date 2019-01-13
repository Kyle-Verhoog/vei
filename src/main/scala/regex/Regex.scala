package regex
import scala.collection.{SortedSet, mutable}
import compiler.scanner.Token.Token
import scala.collection.mutable.{HashMap, ListBuffer}
import compiler.Compiler.State
import compiler.NFA

case class Paren(val nalt: Integer, val natom: Integer) {
}

object Regex {
  def toPostfix(regex: String): String = {
    var postfix = ""
    var cch = '@'
    var x = 0
    var natom = 0
    var nalt = 0
    var parens = new ListBuffer[Paren]()

    for (x <- 0 until regex.length()) {
      val re = regex.charAt(x)

      re match {
        case '(' => {
          if (natom > 1) {
            natom -= 1
            postfix += cch
          }

          parens += new Paren(nalt, natom)
          nalt = 0
          natom = 0
        }
        case '|' => {
          if (natom == 0) {
            println("Error") // TODO: throw
          }
          natom -= 1
          while (natom > 0) {
            postfix += cch
            natom -= 1
          }
          nalt += 1
        }
        case ')' => {
          if (parens.length < 1) {
            println("error 1") // TODO: throw
          }
          if (natom == 0) {
            println("error 2") // TODO: throw
          }

          natom -= 1
          while (natom > 0) {
            postfix += cch
            natom -= 1
          }

          while (nalt > 0) {
            postfix += '|'
            nalt -= 1
          }

          var par = parens.remove(parens.length - 1)
          nalt = par.nalt
          natom = par.natom
          natom += 1
        }
        case '?' | '*' | '+' => {
          if (natom == 0) {
            println("Error symb") // throws
          }
          postfix += re
        }
        case _ => {
          if (natom > 1) {
            natom -= 1
            postfix += cch
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
      postfix += cch
      natom -= 1
    }

    while (nalt > 0) {
      postfix += '|'
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
    alphabet: Set[String],
    transitionTable: mutable.HashMap[(State, String), Set[State]]
    ): NFA[String] = {
    new NFA[String](
      states,
      acceptingStates,
      startStates,
      alphabet,
      transitionTable,
      HashMap[State, Token](),
      "ε"
      )
  }

  def postfixToNFA(postfix: String): NFA[String] = {
    val alpha = Set("a", "b", "c")
    var stack = new ListBuffer[NFA[String]]()
    var x = 0

    for (x <- 0 until postfix.length()) {
      val p = postfix.charAt(x).toString

      p match {
        case "@" => {
          val e2 = stack.remove(stack.length - 1)
          val e1 = stack.remove(stack.length - 1)
          println(e2)
          println(e1)

          var transitionTable = mergeMaps[(State, String), Set[State]](e1.transitionTable, e2.transitionTable)

          for (a <- e1.acceptingStates) {
            transitionTable += ((a, "ε") -> e2.startStates) // TODO: copy e2.startStates
          }

          var newStates = e1.states | e2.states
          var newAcceptingStates = e2.acceptingStates
          var newStartStates = e1.startStates

          var nfa = newNFA(
            newStates,
            newAcceptingStates,
            newStartStates,
            alpha,
            transitionTable,
          )
          stack += nfa
        }
        case _ => {
          val _ps = scala.util.Random.alphanumeric.take(16).mkString
          val ps = scala.util.Random.alphanumeric.take(16).mkString

          var nfa = newNFA(
            Set[State](_ps, ps),
            Set[State](ps),
            Set[State](_ps),
            alpha,
            HashMap((_ps, p) -> Set(ps)),
          )

          stack += nfa
        }
      }
    }
    stack(0)
  }
}
