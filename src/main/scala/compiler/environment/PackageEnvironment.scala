package compiler.environment

import compiler.ast.AST

class PackageEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
