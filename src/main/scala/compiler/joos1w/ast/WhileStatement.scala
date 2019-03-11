package compiler.joos1w.ast

class WhileStatement() extends AST {
  def expr: AST = {
    children.head
  }

  def body: AST = {
    children(1)
  }
}
