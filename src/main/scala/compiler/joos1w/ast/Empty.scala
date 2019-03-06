package compiler.joos1w.ast

class Empty(val fieldName: String = "") extends AST {
  override def strFields: String = {
    s"$fieldName"
  }
}
