package compiler.joos1w.ast

class VariableDeclarator(val name: String) extends AST {
  override def strFields: String = {
    s"$name"
  }
}
