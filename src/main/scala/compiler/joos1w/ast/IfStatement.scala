package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

class IfStatement() extends AST {
  def getTopLevelStatements(): List[AST] = {
    var topLevelStatements = List[AST](this)

    if (children.length == 3) { // pull up
      topLevelStatements = topLevelStatements ++ List(children(2))

      children(2) match {
        case child: IfStatement =>
          topLevelStatements = topLevelStatements ++ child
            .getTopLevelStatements()
        case _ =>
      }

      removeChildFromEnd()
    }

    topLevelStatements
  }

  def expr: AST = {
    children.head
  }

  def body: AST = {
    children(1)
  }

  override def fromParseTree(parseTree: ParseTreeNode[Token]): AST = {
    new AST()
  }
}
