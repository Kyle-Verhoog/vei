package compiler.joos1w.ast.literals

import compiler.joos1w.ast.AST
import compiler.joos1w.environment.types.numeric.IntType

// TODO how to check>?
class IntegerLiteral(rawValue: String, var negative: Boolean = false)
    extends AST {
  val ttype = new IntType

  // used to set if an integer is negative after it is discovered to be negative
  def setNegative(value: Boolean): Unit = {
    negative = value
  }

  def integerValue: Int = {
    // Hack until we fix weeding
    if (rawValue == "2147483648") {
      return -2147483648
    }

    if (negative) {
      return ("-" + rawValue).toInt
    }
    rawValue.toInt
  }

  override def strFields: String = {
    s"$integerValue"
  }
}
