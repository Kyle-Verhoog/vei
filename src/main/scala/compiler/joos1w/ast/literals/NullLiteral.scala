package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST

// TODO maybe check that value is equal to null? probably not needed since scanner does that
class NullLiteral(value: String) extends AST {}
