package compiler.ast

object MethodDeclaration {
  def fromParseTreeNode(): MethodDeclaration = {
    new MethodDeclaration()
  }
}

class MethodDeclaration() extends AST {}
