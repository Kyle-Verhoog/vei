package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

class IfStatement() extends AST {
  // returns the children for the if and else (if present) statements
  def getStatementChildren: List[AST] = {
    val ifStatementBody = getChild(1)

    if (ifStatementBody.isEmpty) return List()

    try {
      val elseStatementBody = getChild(2)
      List(ifStatementBody.get, elseStatementBody.get)
    } catch {
      case _: Throwable => List(ifStatementBody.get)
    }

  }

  override def fromParseTree(parseTree: ParseTreeNode[Token]): AST = {
    new AST()
  }
}
