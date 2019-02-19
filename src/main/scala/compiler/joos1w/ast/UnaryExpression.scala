package compiler.joos1w.ast

class UnaryExpression(operator: String) extends AST {
  override def strFields: String = {
    s"$operator"
  }
}
