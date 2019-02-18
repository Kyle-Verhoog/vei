package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST

class BooleanLiteral(value: Boolean) extends AST {
  override def strFields: String = {
    s"$value"
  }
}
