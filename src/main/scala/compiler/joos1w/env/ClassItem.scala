package compiler.joos1w.env

import compiler.joos1w.ast.AST

abstract class ClassItem(var parent: PackageItem, ast: AST) extends Env {
  val name: ClassItemName
}
