package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST

class StringLiteral(val value: String) extends AST {

  override def strFields: String = {
    value
  }
}
