package compiler.joos1w.ast

class ImportDeclaration() extends AST {
  def name: String = {
    children.head.asInstanceOf[Name].name
  }
}
