package compiler.joos1w.ast

class UnaryExpression(val operator: String) extends AST {
  def subExpression: AST = {
    if (children.length != 1) {
      throw new RuntimeException(
        "Expected 1 child for unary expression, got: " + children.length)
    }
    children.head
  }

  override def strFields: String = {
    s"$operator"
  }
}
