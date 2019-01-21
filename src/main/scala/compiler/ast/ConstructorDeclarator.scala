package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object ConstructorDeclarator {
  def fromParseTreeNode(name: ParseTreeNode[Token]): ConstructorDeclarator = {
    new ConstructorDeclarator(
      name = AST.getValue(name)
    )
  }
}

class ConstructorDeclarator(name: String) extends AST {
  // TODO: vet that the constructor name matches the encompassing class name
}
