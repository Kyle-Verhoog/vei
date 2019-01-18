package compiler.parser

import compiler.scanner.Token

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Parser {
  class CFG(
      val terminals: ListBuffer[String],
      val nonterminal: ListBuffer[String],
      val prodRules: mutable.HashMap[String, ListBuffer[Array[String]]],
      val shiftActions: mutable.HashMap[Int, mutable.HashMap[String, Int]],
      val reduceActions: mutable.HashMap[Int, mutable.HashMap[String, Int]]
  )

  class Tree[T](
      val token: T,
      val children: ListBuffer[Tree[T]] = ListBuffer[Tree[T]]()
  )

  def parse(cfg: CFG, tokens: ListBuffer[Token]): Unit = {
    val nodeStack = ListBuffer[Tree[Token]]()
    val stateStack = ListBuffer[Int]()

    // start algorithm
    nodeStack.append(new Tree[Token](new Token("START", "none")))
    stateStack.append(0, -1) // TODO need -1 state
    for (token <- tokens) {

      // TODO other condition
      while (cfg.reduceActions.contains(stateStack.last)) {
        val newState = cfg.reduceActions(stateStack.last)(token.value)
        val A = 





        val tokens = cfg.reduceActions(stateStack.last)
        val gamma = tokens.size

        // pop things off the nodestack
        val childNodes = nodeStack.takeRight(gamma).reverse
        nodeStack.remove(nodeStack.size - gamma, gamma)

        // TODO add stuff
        //nodeStack.append(new Tree[Token](, childNodes))
      }
    }

  }

  def readInLr1(file: String): Unit = {
    var lines = Source.fromResource("grammar.lr1").getLines().toArray

    // ---- process terminals ----
    var count = lines(0).toInt
    val terminals = ListBuffer[String]()
    lines = lines.takeRight(lines.length - 1)

    for (i <- 0 until count) {
      terminals.append(lines(0))
      lines = lines.takeRight(lines.length - 1)
    }

    // ---- process non-terminals ----
    count = lines(0).toInt
    val nonterminals = ListBuffer[String]()
    lines = lines.takeRight(lines.length - 1)

    for (i <- 0 until count) {
      nonterminals.append(lines(0))
      lines = lines.takeRight(lines.length - 1)
    }

    // ---- process production rules ----
    count = lines(0).toInt
    val prodRules = mutable.HashMap[String, ListBuffer[Array[String]]]()
    lines = lines.takeRight(lines.length - 1)

    for (i <- 0 until count) {
      val line = lines(0).split(" ")
      val key = line(0)
      val value = line.takeRight(line.length - 1)

      // make entry if needed
      if (!prodRules.contains(key)) {
        prodRules(key) = ListBuffer[Array[String]]()
      }
      prodRules(key).append(value)
    }

    // ---- process actions ----
    count = lines(1).toInt
    lines = lines.takeRight(lines.length - 2)
    val shiftingMap = mutable.HashMap[Int, mutable.HashMap[String, Int]]()
    val reduceMap = mutable.HashMap[Int, mutable.HashMap[String, Int]]()

    for (i <- 0 until count) {
      val line = lines(0).split(" ")
      val currentState = line(0).toInt
      val token = line(1)
      val action = line(2)
      val toState = line(3).toInt

      if (!shiftingMap.contains(currentState)) { // initialize the maps
        shiftingMap(currentState) = mutable.HashMap[String, Int]()
        reduceMap(currentState) = mutable.HashMap[String, Int]()
      }

      if (action == "shift") { // add the shift
        shiftingMap(currentState) += token -> toState
      } else { // add the reduce
        reduceMap(currentState) += token -> toState
      }

      // move to next line
      lines = lines.takeRight(lines.length - 1)
    }

    new CFG(
      terminals,
      nonterminals,
      prodRules,
      shiftingMap,
      reduceMap
    )
  }
}
