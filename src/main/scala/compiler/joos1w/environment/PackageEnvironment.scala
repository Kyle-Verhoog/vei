package compiler.joos1w.environment

import compiler.joos1w.ast.AST

class PackageEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {
  override def searchForSimpleClass(name: String): Option[AST] = {
    classTable.get(name)
  }
}
