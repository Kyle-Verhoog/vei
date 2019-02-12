package compiler.joos1w.environment

import compiler.joos1w.ast.AST

class PackageEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
