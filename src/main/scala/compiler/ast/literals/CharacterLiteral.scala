package compiler.ast.literals

import compiler.ast.AST

// TODO find a nice way to convert strings to characters, or just manually do something weird
class CharacterLiteral(val value: String) extends AST {

}
