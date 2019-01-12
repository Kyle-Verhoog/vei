package compiler.scanner

object Token extends Enumeration {
  type Token = Value
  val IF, ELSE, INTEGER = Value
}
