package compiler.joos1w.env

import compiler.joos1w.ast._

class Field(parent: PackageItem, ast: FieldDeclaration)
    extends ClassItem(parent, ast) {
  val name: FieldName =
    new FieldName(parent.name, ast.modifiers, ast.fieldType, ast.name)

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    None
  }

  override def toStrTree: String = {
    s"Field(name: $name)"
  }
}
