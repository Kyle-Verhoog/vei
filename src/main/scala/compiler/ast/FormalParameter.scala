package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object FormalParameter {
  def fromParseTreeNode(
      formalParameter: ParseTreeNode[Token]): FormalParameter = {
    new FormalParameter(
      name = AST.getValue(formalParameter.children(1))
    )
  }
}

class FormalParameter(val name: String) extends AST {
  def ttype: String = {
    getChild(0).asInstanceOf[Type].ttype
  }
}
