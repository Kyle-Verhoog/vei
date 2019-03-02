package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object MethodDeclarator {
  def fromParseTreeNode(identifier: ParseTreeNode[Token]): MethodDeclarator = {
    new MethodDeclarator(
      identifier = AST.getValue(identifier)
    )
  }
}

class MethodDeclarator(val identifier: String) extends AST {
  def parameters: ASTList = {
    if (children.last.isInstanceOf[Empty]) return new ASTList()
    children.last.asInstanceOf[ASTList]
  }

  override def strFields: String = {
    s"$identifier"
  }
}
