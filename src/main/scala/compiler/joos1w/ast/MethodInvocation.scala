package compiler.joos1w.ast

class MethodInvocation() extends AST {
  def simpleName: String = {
    children(1).asInstanceOf[Name].name
  }

  def argumentList: ASTList = {
    children.head.asInstanceOf[ASTList]
  }

  def qualifiedName: String = {
    if (children.length > 2) {
      children.last.asInstanceOf[Name].name + "." + simpleName
    } else {
      simpleName
    }
  }
}
