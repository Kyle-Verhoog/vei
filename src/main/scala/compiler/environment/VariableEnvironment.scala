package compiler.environment

import compiler.ast.AST

class VariableEnvironment(val myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
