package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.types.StringType

class StringLiteral(val value: String) extends AST {
  val ttype = new StringType

  override def strFields: String = {
    value
  }
}
