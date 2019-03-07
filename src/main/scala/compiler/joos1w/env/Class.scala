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

  override def lookup(name: Name): Option[Env] = {
    parent.lookup(name)
  }

  override def toString: String = {
    val (nmeths, nfields, ncons) =
      namespace.keys.foldLeft((0, 0, 0))({
        case ((nmeths, nfields, ncons), name) =>
          name match {
            case _: MethodName      => (nmeths + 1, nfields, ncons)
            case _: FieldName       => (nmeths, nfields + 1, ncons)
            case _: ConstructorName => (nmeths, nfields + 1, ncons)
          }
      })
    s"Class(name: $name, nmethods:$nmeths, nfields:$nfields)"
  }
}
