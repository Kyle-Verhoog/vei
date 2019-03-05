package compiler.joos1w.env

import compiler.joos1w.ast.ClassDeclaration

class Class(val parent: Package, ast: ClassDeclaration) extends Env {
  def name: String = {
    ast.identifier
  }

  override def globalLookup(qualifiedName: String): Option[Env] = {
    parent.globalLookup(qualifiedName)
  }
}
