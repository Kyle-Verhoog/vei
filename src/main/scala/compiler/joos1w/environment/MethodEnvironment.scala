package compiler.joos1w.environment

import compiler.joos1w.ast.{AST, AbstractMethodDeclaration, ConstructorDeclaration, MethodDeclaration}
import compiler.joos1w.environment.environment.Signature

class MethodEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  def signature: Signature = {
    myAst match {
      case ast: MethodDeclaration => ast.signature
      case ast: ConstructorDeclaration => ast.signature
      case ast: AbstractMethodDeclaration => ast.signature
      case _ => throw new RuntimeException("Unknown ast type for method env" + ast)
    }
  }

  def modifiers: List[String] = {
    myAst match {
      case ast: MethodDeclaration => ast.modifiers
      case ast: ConstructorDeclaration => ast.modifiers
      case ast: AbstractMethodDeclaration => ast.modifiers
      case _ => throw new RuntimeException("Unknown ast type for method env" + ast)
    }
  }
}
