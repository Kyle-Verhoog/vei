package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object FormalParameter {
  def fromParseTreeNode(ttype: ParseTreeNode[Token]): FormalParameter = {
    new FormalParameter(
      ttype = AST.getValue(ttype)
    )
  }
}

class FormalParameter(ttype: String) extends AST {}
