package compiler.ast.literals

import compiler.ast.AST

// TODO how to check>?
class IntegerLiteral(rawValue: String, var negative: Boolean = false) extends AST {

  // used to set if an integer is negative after it is discovered to be negative
  def setNegative(value: Boolean): Unit = {
    negative = value
  }

  def integerValue: Int = {
    if (negative) {
      return ("-" + rawValue).toInt
    }
    rawValue.toInt
  }
}
