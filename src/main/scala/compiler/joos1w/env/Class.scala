package compiler.joos1w.env

import compiler.joos1w.ast.ClassDeclaration

class Class(val parent: Package, ast: ClassDeclaration) extends Env {
  val name = new ClassName(parent.name, ast.identifier)

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(qualifiedName: Name): Option[Env] = {
    parent.lookup(qualifiedName)
  }

  override def toString: String = {
    s"ClassEnv($name)"
  }

  override def toStrTree: String = {
    toString
  }
}
