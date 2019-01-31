package compiler.environment

import compiler.ast.AST

// used for things like for, while, blocks, etc...
class BlockEnvironment(myAst: AST, parent: Option[GenericEnvironment])
    extends GenericEnvironment(myAst, parent) {}
