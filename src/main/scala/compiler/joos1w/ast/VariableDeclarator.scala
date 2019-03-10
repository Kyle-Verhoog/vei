package compiler.joos1w.ast

class VariableDeclarator(val name: String) extends AST {
  def hasExpression: Boolean = {
    children.nonEmpty
  }

  def expression: AST = {
    children.head
  }

  override def strFields: String = {
    s"$name"
  }
}
