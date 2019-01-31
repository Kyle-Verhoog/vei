package compiler.ast

class PackageDeclaration() extends AST {
  def name: String = {
    leftChild.get.asInstanceOf[Name].name
  }
}
