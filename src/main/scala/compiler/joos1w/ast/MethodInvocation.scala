package compiler.joos1w.ast

class MethodInvocation(val id: Option[String]) extends AST {
  var returnType: Option[String] = None

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
    children.head.asInstanceOf[ASTList]
  }

  def primary: Option[AST] = {
    children.last match {
      case ast: Name => None
      case ast       => Some(ast)
    }
  }
}
