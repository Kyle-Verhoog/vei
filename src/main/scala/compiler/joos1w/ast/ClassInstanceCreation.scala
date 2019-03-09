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
}
