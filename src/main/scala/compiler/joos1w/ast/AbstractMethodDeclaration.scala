package compiler.joos1w.ast

class AbstractMethodDeclaration extends AST {
  def identifier: String = {
    this.getDescendant(2, Some(1)) match {
      case Some(n: MethodDeclarator) => n.identifier
      case e =>
        throw MalformedASTException(
          s"AbstractMethodDeclaration does not have MethodDeclarator child (got $e."
        )
    }
  }
}
