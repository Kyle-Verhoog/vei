package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object FormalParameter {
  def fromParseTreeNode(
      formalParameter: ParseTreeNode[Token]): FormalParameter = {
    new FormalParameter(
      ttype = AST.getValue(formalParameter.children(0)),
      name = AST.getValue(formalParameter.children(1))
    )
  }
}

class FormalParameter(ttype: String, val name: String) extends AST {}
