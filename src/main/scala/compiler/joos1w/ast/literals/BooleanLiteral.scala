package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.types.BooleanType

class BooleanLiteral(val value: Boolean) extends AST {
  val ttype = new BooleanType

  override def strFields: String = {
    s"$value"
  }
}
