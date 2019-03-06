package compiler.joos1w.env

import compiler.joos1w.ast.{AST, ClassDeclaration, Empty, PackageDeclaration}

abstract class PackageItem() extends Env {
  val name: QualifiedName
}
