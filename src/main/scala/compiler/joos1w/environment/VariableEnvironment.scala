package compiler.joos1w.environment

import compiler.joos1w.ast.{AST, FieldDeclaration, FormalParameter, LocalVariableDeclaration}

class VariableEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  def name: String = {
    myAst match {
      case ast: LocalVariableDeclaration => ast.name
      case ast: FieldDeclaration => ast.name
      case ast: FormalParameter => ast.name
      case _ => throw new RuntimeException("Unknown ast type for variable env")
    }
  }
}
