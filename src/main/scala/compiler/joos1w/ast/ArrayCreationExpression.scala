package compiler.joos1w.ast

class ArrayCreationExpression() extends AST {
  def primary: AST = {
    children.head
  }

  def expr: AST = {
    children(1)
  }
}
