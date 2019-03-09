package compiler.joos1w.ast

class Assignment extends AST {

  def getLHS: AST = {
    children.head
  }

  // TODO not needed probably
  def getOperator: String = {
    "="
  }

  def getRHS: AST = {
    children(1)
  }
}
