package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object ConstructorDeclarator {
  def fromParseTreeNode(name: ParseTreeNode[Token]): ConstructorDeclarator = {
    new ConstructorDeclarator(
      name = AST.getValue(name)
    )
  }
}

class ConstructorDeclarator(val name: String) extends AST {
  // TODO: vet that the constructor name matches the encompassing class name

  def params: List[(String, String)] = {
    var ps: List[(String, String)] = List()
    parameters.children.foreach((c: AST) =>
      c match {
        case c: FormalParameter => ps = (c.ttype, c.name) :: ps
        case _                  => throw new RuntimeException("should not happen")
    })
    ps
  }

  def parameters: ASTList = {
    if (children.last.isInstanceOf[Empty]) return new ASTList()
    children.last.asInstanceOf[ASTList]
  }
}
