package compiler.ast.literals

import compiler.ast.AST

// TODO maybe check that value is equal to null? probably not needed since scanner does that
class NullLiteral(value: String) extends AST {

}
