package compiler.ast

class LocalVariableDeclaration() extends AST {
  def ttype: String = {
    getChild(0).asInstanceOf[Type].ttype
  }

  def name: String = {
    getChild(1).get.asInstanceOf[VariableDeclarator].name
  }
}
