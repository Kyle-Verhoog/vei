package compiler.parser

import compiler.scanner.Token

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Parser {
  class CFG(
      val terminals: ListBuffer[String],
      val nonterminal: ListBuffer[String],
      val prodRules: ListBuffer[Array[String]],
      val shiftActions: mutable.HashMap[Int, mutable.HashMap[String, Int]],
      val reduceActions: mutable.HashMap[Int, mutable.HashMap[String, Int]]
  )

  class Tree[T](
      val token: T,
      val children: ListBuffer[Tree[T]] = ListBuffer[Tree[T]]()
  )

  def parse(cfg: CFG, tokens: ListBuffer[Token]): ListBuffer[Tree[Token]] = {
    var nodeStack = ListBuffer[Tree[Token]]()
    var stateStack = ListBuffer[Int]()

    // start algorithm
    nodeStack.append(new Tree[Token](new Token("BOF", "none"))) // TODO dont hardcode BOF?
    stateStack.append(0)
    for (token <- tokens) {

      while (cfg.reduceActions.contains(stateStack.last) && cfg
               .reduceActions(stateStack.last)
               .contains(token.value)) {
        // get the production rule and split into parts
        val ruleToReduce = cfg.reduceActions(stateStack.last)(token.value)
        val prodRule = cfg.prodRules(ruleToReduce)
        val A = prodRule(0) // look up the nonterminal that this production rule is
        val gamma = prodRule.takeRight(prodRule.length - 1)

        // pop |gamma| child nodes, and |gama| states
        val childNodes = ListBuffer[Tree[Token]]()
        for (i <- 0 until gamma.length) {
          childNodes.append(nodeStack.last)
          nodeStack = nodeStack.take(nodeStack.length - 1)
          stateStack = stateStack.take(stateStack.length - 1)
        }

        nodeStack.append(new Tree[Token](new Token(A, "non-leaf"), childNodes))

        if (!cfg.shiftActions.contains(stateStack.last) || !cfg.shiftActions(stateStack.last).contains(A)) {
          throw new RuntimeException("bad parsing 1") // TODO better error
        }

        stateStack.append(cfg.shiftActions(stateStack.last)(A))
      }

      nodeStack.append(new Tree[Token](token))

      // Error check
      if (!cfg.shiftActions.contains(stateStack.last) || !cfg.shiftActions(stateStack.last).contains(token.value)) {
        throw new RuntimeException("bad parsing 2") // TODO better error
      }

      stateStack.append(cfg.shiftActions(stateStack.last)(token.value))
    }

    nodeStack
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
    val prodRules = ListBuffer[Array[String]]()
    lines = lines.takeRight(lines.length - 1)

    for (i <- 0 until count) {
      prodRules.append(lines(0).split(" "))
      lines = lines.takeRight(lines.length - 1)
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
