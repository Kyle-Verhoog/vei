package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object ConstructorDeclaration {
  def fromParseTreeNode(
      modifiers: ParseTreeNode[Token]): ConstructorDeclaration = {
    new ConstructorDeclaration(
      modifiers = AST.getValueList(modifiers)
    )
  }
}

class ConstructorDeclaration(modifiers: List[String]) extends AST {}
