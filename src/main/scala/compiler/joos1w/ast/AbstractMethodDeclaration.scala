package compiler.joos1w.ast

import compiler.joos1w.environment.environment.Signature

class AbstractMethodDeclaration extends ASTMethodDeclaration {
  def signature: Signature = {
    if (header.isDefined) return header.get.signature
    (identifier.split('.').last, Some(List()))
  }

  override def identifier: String = {
    this.getDescendant(2, Some(1)) match {
      case Some(n: MethodDeclarator) => n.identifier
      case e =>
        throw MalformedASTException(
          s"AbstractMethodDeclaration does not have MethodDeclarator child (got $e."
        )
    }
  }
}
