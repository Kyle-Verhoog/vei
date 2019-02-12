package compiler.joos1w.environment

import compiler.joos1w.ast.AST

class MethodEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
