package compiler.joos1w.environment
import compiler.joos1w.ast.AST

class RootEnvironment(val myAst: AST, val parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
