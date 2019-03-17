package compiler.joos1w.ast

class ConditionalExpression(val operator: String) extends AST {
  def firstExpr: AST = {
    children.head
  }

  def secondExpr: AST = {
    children(1)
  }

  override def strFields: String = {
    s"$operator"
  }
}
