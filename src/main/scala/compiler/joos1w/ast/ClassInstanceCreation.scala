package compiler.joos1w.ast

class ClassInstanceCreation extends AST {
  def name: String = {
    children.head match {
      case name: Name => name.name
      case _ =>
        throw new RuntimeException("Class Instance Creation needs a class")
    }
  }

  def primary: Name = {
    children.head.asInstanceOf[Name]
  }


  def parameters: List[AST] = {
    children.last match {
      case child: ASTList => child.children
      case child: Empty   => List()
      case _              => List(children.last)
    }
  }

  def argumentList: ASTList = {
    children.last match {
      case child: ASTList => child
      case _              => throw new RuntimeException("malformed AST")
    }
  }
}
