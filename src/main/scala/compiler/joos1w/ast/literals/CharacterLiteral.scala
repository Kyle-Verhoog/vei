package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.types.numeric.CharType

// TODO find a nice way to convert strings to characters, or just manually do something weird
class CharacterLiteral(val value: String) extends AST {
  val ttype = new CharType
}
