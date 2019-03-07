package compiler.joos1w.env

import compiler.joos1w.ast._

class Method(parent: PackageItem, ast: ASTMethodDeclaration)
    extends ClassItem(parent, ast) {
  val name: MethodName = ast match {
    case ast: AbstractMethodDeclaration =>
      new MethodName(ast.modifiers,
                     ast.returnType,
                     ast.identifier,
                     ast.header.get.params)
    case ast: MethodDeclaration =>
      new MethodName(ast.modifiers,
                     ast.returnType,
                     ast.identifier,
                     ast.header.get.params)
    case _ => throw new RuntimeException("Unexpected method ast node")
  }

  override def globalLookup(name: Name): Option[Env] = {
    parent.globalLookup(name)
  }

  override def lookup(name: Name): Option[Env] = {
    None
  }
}
