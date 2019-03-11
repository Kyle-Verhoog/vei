package compiler.joos1w.ast

class Return() extends AST {
  def expr(): AST = {
    children.head
  }
}
