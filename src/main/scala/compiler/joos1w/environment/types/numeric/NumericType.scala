package compiler.joos1w.environment.types.numeric

import compiler.joos1w.environment.types.AbstractType

abstract class NumericType extends AbstractType {
  override def isNumeric: Boolean = true
}
