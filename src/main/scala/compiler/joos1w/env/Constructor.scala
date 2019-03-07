package compiler.joos1w.env

import compiler.joos1w.ast.{ConstructorDeclaration}

class Constructor(parent: PackageItem, ast: ConstructorDeclaration)
    extends ClassItem(parent, ast) {
  val name: MethodName = new MethodName(
    ast.modifiers,
    ast.returnType,
    ast.identifier,
    ast.header.get.params
  )

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    None
  }
}
