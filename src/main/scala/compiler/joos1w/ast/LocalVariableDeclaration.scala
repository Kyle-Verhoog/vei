package compiler.joos1w.ast

class LocalVariableDeclaration() extends AST {
  def ttype: String = {
    getChild(0) match {
      case Some(t: Type) => t.ttype
      case _             => throw new RuntimeException()
    }
  }

  def name: String = {
    getChild(1) match {
      case Some(t: VariableDeclarator) => t.name
      case _                           => throw new RuntimeException()
    }
  }

  override def strFields: String = {
    s"$ttype $name"
  }
}
