package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object MethodDeclarator {
  def fromParseTreeNode(identifier: ParseTreeNode[Token]): MethodDeclarator = {
    new MethodDeclarator(
      identifier = AST.getValue(identifier)
    )
  }
}

class MethodDeclarator(val identifier: String) extends AST {}
