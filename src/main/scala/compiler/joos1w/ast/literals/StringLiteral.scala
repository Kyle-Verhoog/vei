package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.types.StringType

class StringLiteral(val value: String) extends AST {
  lazy val ttype = new StringType(this.env)

  override def strFields: String = {
    value
  }
}
