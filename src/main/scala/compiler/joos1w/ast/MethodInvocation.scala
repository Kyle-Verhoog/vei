package compiler.joos1w.ast

import compiler.joos1w.environment.MethodEnvironment

class MethodInvocation(val id: Option[String]) extends AST {
  var returnType: Option[String] = None
  var methodDefinition: Option[MethodEnvironment] = None

  def name: String = {
    if (id.isDefined) return id.get
    children.last.asInstanceOf[Name].name
  }

  def parameters: List[AST] = {
    children.head match {
      case child: ASTList => child.children
      case child: Empty   => List()
      case _              => List(children.head)
    }
  }

  def argumentList: ASTList = {
    children.head match {
      case child: ASTList => child
      case _ => throw new RuntimeException("malformed AST")
    }
  }

  def primary: Option[AST] = {
    children.last match {
      case ast: Name => None
      case ast       => Some(ast)
    }
  }
}
