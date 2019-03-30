package compiler.joos1w.environment

import compiler.joos1w.ast.AST

class LengthEnvironmentt(myAst: AST = null,
                         val parent: Option[GenericEnvironment] = None)
    extends VariableEnvironment(myAst, parent) {}
