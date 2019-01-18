package compiler.parser

import compiler.scanner.Token

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Parser {
  class CFG(
      val terminals: ListBuffer[String],
      val nonterminal: ListBuffer[String],
      val prodRules: ListBuffer[List[String]],
      val shiftActions: mutable.HashMap[Int, mutable.HashMap[String, Int]],
      val reduceActions: mutable.HashMap[Int, mutable.HashMap[String, Int]]
  )

  class Tree[T](
      val token: T,
      val children: ListBuffer[Tree[T]] = ListBuffer[Tree[T]]()
  ) {
    def inorderLeafValues(): ListBuffer[T] = {
      if (children.isEmpty) return ListBuffer(token)

      children.flatMap(child => { child.inorderLeafValues() })
    }
  }

  def parse(cfg: CFG,
            inputTokens: ListBuffer[Token]): ListBuffer[Tree[Token]] = {
    var tokens = inputTokens
    var nodeStack = ListBuffer[Tree[Token]]()
    var stateStack = ListBuffer[Int]()

    // start algorithm
    stateStack.append(cfg.shiftActions(0)(tokens.head.tokenType)) // push the first state

    // push tokens.head to node stack and pop it from tokens
    nodeStack.append(new Tree[Token](tokens.head))
    tokens = tokens.takeRight(tokens.length - 1)

    for (i <- tokens.indices) {
      val token = tokens(i)

      while (cfg.reduceActions.contains(stateStack.last) && cfg
               .reduceActions(stateStack.last)
               .contains(token.tokenType)) {
        // get the production rule and split into parts
        val ruleToReduce = cfg.reduceActions(stateStack.last)(token.tokenType)
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

        if (!cfg.shiftActions.contains(stateStack.last) || !cfg
              .shiftActions(stateStack.last)
              .contains(A)) {
          println(tokens.take(10))
          throw new RuntimeException("bad parsing 1") // TODO better error
        }

        stateStack.append(cfg.shiftActions(stateStack.last)(A))
      }

      nodeStack.append(new Tree[Token](token))

      // Error check
      if (!cfg.shiftActions.contains(stateStack.last) || !cfg
            .shiftActions(stateStack.last)
            .contains(token.tokenType)) {
        println("Cant find shift transition from state: " + stateStack.last + " on token: " + token)
        println("Prev 10 tokens: " + tokens.slice(i-10, i))
        throw new RuntimeException("bad parsing 2") // TODO better error
      }

      stateStack.append(cfg.shiftActions(stateStack.last)(token.tokenType))
    }

    nodeStack
  }

  def readInLr1(fileLines: Array[String]): CFG = {
    var lines = fileLines

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

    // ---- ignore start symbol ----
    lines = lines.takeRight(lines.length - 1)

    // ---- process production rules ----
    count = lines(0).toInt
    val prodRules = ListBuffer[List[String]]()
    lines = lines.takeRight(lines.length - 1)

    for (i <- 0 until count) {
      prodRules.append(lines(0).split(" ").toList)
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
