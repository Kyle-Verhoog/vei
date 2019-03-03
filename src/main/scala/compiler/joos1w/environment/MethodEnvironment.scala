package compiler.joos1w.environment

import compiler.joos1w.ast.{AST, ConstructorDeclaration, MethodDeclaration}
import compiler.joos1w.environment.environment.Signature

class MethodEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  def signature: Signature = {
    myAst match {
      case ast: MethodDeclaration => ast.signature
      case ast: ConstructorDeclaration => ast.signature
      case _ => throw new RuntimeException("Unknown ast type for method env")
    }
  }
}
