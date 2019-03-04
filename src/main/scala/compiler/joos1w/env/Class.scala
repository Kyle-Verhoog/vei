package compiler.joos1w.env

import compiler.joos1w.ast.ClassDeclaration


object Class {
  def fromAST(parent: Package, ast: ClassDeclaration): Class = {
    val name = ast.identifier
    new Class(name, parent)
  }
}

class Class(val name: String, val parent: Package) extends Env {
}