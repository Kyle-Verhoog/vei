package compiler.joos1w.ast

class ImportDeclarationsList extends ASTList("import declarations") {

  def getImports = {
    children.map(child => child.asInstanceOf[ImportDeclaration])
  }
}
