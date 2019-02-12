package compiler.joos1w.environment

import compiler.joos1w.ast.AST

class VariableEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
