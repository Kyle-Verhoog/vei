package compiler.joos1w.ast

import compiler.joos1w.environment.types
import compiler.joos1w.environment.types.{AbstractType, BooleanType}

class PrimitiveType(val typeName: String) extends AST {
  def ttype: AbstractType = {
    types.buildTypeFromString(typeName, this.env)
  }
  override def strFields: String = {
    s"$typeName"
  }
}
