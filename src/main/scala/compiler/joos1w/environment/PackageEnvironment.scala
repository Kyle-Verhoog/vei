package compiler.joos1w.environment

import compiler.joos1w.ast.AST

class PackageEnvironment(val myAst: AST, parent: Option[GenericEnvironment], val pkgName: String)
    extends GenericEnvironment(myAst, parent) {
  override def searchForSimpleClass(name: String): Option[ClassEnvironment] = {
    //println("searching in pkg env for class " + name)
    classTable.get(name)
  }
}
