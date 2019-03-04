package compiler.joos1w.environment

import compiler.joos1w.ast._
import compiler.joos1w.environment.environment.Signature
import exceptions.EnvironmentError

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
    var implicitModifiers = List[String]()
    // if parent is an interface, implicitly add public abstract
    if (parentEnvironment.get.asInstanceOf[ClassEnvironment].ast.isInstanceOf[InterfaceDeclaration]) {
      implicitModifiers = List("abstract", "public")
    }

    myAst match {
      case ast: MethodDeclaration => ast.modifiers ++ implicitModifiers
      case ast: ConstructorDeclaration => ast.modifiers ++ implicitModifiers
      case ast: AbstractMethodDeclaration => ast.modifiers ++ implicitModifiers
      case _ => throw new RuntimeException("Unknown ast type for method env" + ast)
    }
  }

  def returnType: String = {
    myAst match {
      case ast: MethodDeclaration => ast.returnType
      case ast: ConstructorDeclaration => ast.returnType
      case ast: AbstractMethodDeclaration => ast.returnType
      case _ => throw new RuntimeException("Unknown ast type for method env" + ast)
    }
  }

  override def verifyVariable(name: String): Unit = {
    if (variableTable.contains(name)) {
      throw EnvironmentError(
        "Local variable: " + name + " already declared in current scope")
    }

    if (parentEnvironment.isDefined) parentEnvironment.get.verifyVariable(name)
  }
}
