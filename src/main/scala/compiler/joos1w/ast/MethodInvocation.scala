package compiler.joos1w.ast

class MethodInvocation(id: Option[String]) extends AST {
  def name: String = {
    if (id.isDefined) return id.get
    children.last.asInstanceOf[Name].name
  }

  def argumentList: ASTList = {
    children.head.asInstanceOf[ASTList]
  }

  def primary: Option[AST] = {
    children.last match {
      case ast: Name => None
      case ast => Some(ast)
    }
  }
}
