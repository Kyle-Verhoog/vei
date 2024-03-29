package compiler.joos1w.environment

import compiler.joos1w.ast._
import compiler.joos1w.environment.types.AbstractType

// order is the order in whcih this variable was inserted into its parent environment
// mainly used to know which fields come first since we dont keep the hierarchy for fields
class VariableEnvironment(val myAst: AST,
                          parent: Option[GenericEnvironment],
                          var order: Int = 0)
    extends GenericEnvironment(myAst, parent) {
  var fieldOffset = 0

  def name: String = {
    myAst match {
      case ast: LocalVariableDeclaration => ast.name
      case ast: FieldDeclaration         => ast.name
      case ast: FormalParameter          => ast.name
      case _                             => throw new RuntimeException("Unknown ast type for variable env")
    }
  }

  def isSimpleName: Boolean = name.split('.').length == 1

  def ttype: String = {
    myAst match {
      case ast: LocalVariableDeclaration => ast.ttype
      case ast: FieldDeclaration         => ast.fieldType
      case ast: FormalParameter          => ast.ttype
      case _                             => throw new RuntimeException("Unknown ast type for variable env")
    }
  }

  def abstractType: AbstractType = {
    types.buildTypeFromString(ttype, this)
  }

  def modifiers: List[String] = {
    var implicitModifiers = List[String]()
    // if parent is an interface, implicitly add public abstract
    if (parentEnvironment.get
          .isInstanceOf[ClassEnvironment] && parentEnvironment.get
          .asInstanceOf[ClassEnvironment]
          .ast
          .isInstanceOf[InterfaceDeclaration]) {
      implicitModifiers = List("static")
    }

    myAst match {
      case ast: FieldDeclaration         => ast.modifiers ++ implicitModifiers
      case ast: FormalParameter          => List()
      case ast: LocalVariableDeclaration => List()
      case ast =>
        throw new RuntimeException(
          "variable type does not have modifiers " + ast)
    }
  }
}
