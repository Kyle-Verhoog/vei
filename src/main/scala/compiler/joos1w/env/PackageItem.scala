package compiler.joos1w.env

import compiler.joos1w.ast.AST

abstract class PackageItem(val parent: Package, ast: AST) extends Env {
  val name: QualifiedName
}
