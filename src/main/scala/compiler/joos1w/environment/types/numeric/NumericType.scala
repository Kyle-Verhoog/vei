package compiler.joos1w.environment.types.numeric

import compiler.joos1w.environment.types.PrimType

abstract class NumericType extends PrimType {
  override def isNumeric: Boolean = true
}
