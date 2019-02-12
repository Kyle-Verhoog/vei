package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST

// TODO find a nice way to convert strings to characters, or just manually do something weird
class CharacterLiteral(val value: String) extends AST {}
