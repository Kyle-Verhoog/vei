package compiler.joos1w.environment

import compiler.joos1w.ast._

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

  def modifiers: List[String] = {
    var implicitModifiers = List[String]()
    // if parent is an interface, implicitly add public abstract
    if (parentEnvironment.get.asInstanceOf[ClassEnvironment].ast.isInstanceOf[InterfaceDeclaration]) {
      implicitModifiers = List("static")
    }

    myAst match {
      case ast: FieldDeclaration => ast.modifiers ++ implicitModifiers
      case _ => throw new RuntimeException("variable type does not have modifiers")
    }
  }
}
