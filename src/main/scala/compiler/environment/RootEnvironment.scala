package compiler.environment
import compiler.ast.AST

class RootEnvironment(val myAst: AST, val parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
