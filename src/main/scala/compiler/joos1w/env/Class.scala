package compiler.joos1w.env

import compiler.joos1w.ast.ClassDeclaration

class Class(parent: Package, ast: ClassDeclaration)
    extends PackageItem(parent, ast) {
  val name: ClassName = new ClassName(parent.name, ast.identifier)

  def getItem(name: Name): Option[Env] = {
    parent.getItem(name)
  }

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(qualifiedName: Name): Option[Env] = {
    parent.lookup(qualifiedName)
  }

  override def toString: String = {
    s"Class($name)"
  }

  override def toStrTree: String = {
    toString
  }
}
