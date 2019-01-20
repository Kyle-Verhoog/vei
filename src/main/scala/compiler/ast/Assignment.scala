package compiler.ast

import compiler.ast.AST.AST

class Assignment extends AST {

  def getLHS: AST = {
    leftChild.get
  }

  // TODO not needed probably
  def getOperator: String = {
    "="
  }

  def getRHS: AST = {
    getLHS.leftChild.get
  }
}
