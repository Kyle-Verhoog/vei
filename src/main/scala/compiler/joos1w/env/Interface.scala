package compiler.joos1w.env

import compiler.joos1w.ast.{ClassDeclaration, InterfaceDeclaration}

class Interface(override val parent: Package, ast: InterfaceDeclaration)
    extends PackageItem(parent, ast) {
  val name: InterfaceName = new InterfaceName(parent.name, ast.identifier)

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(qualifiedName: Name): Option[Env] = {
    parent.lookup(qualifiedName)
  }

  override def toString: String = {
    s"Interface($name)"
  }

  override def toStrTree: String = {
    toString
  }
}
