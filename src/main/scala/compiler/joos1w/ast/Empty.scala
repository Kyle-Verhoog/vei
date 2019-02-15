package compiler.joos1w.ast

class Empty(fieldName: String = "") extends AST {
  override def strFields: String = {
    s"$fieldName"
  }
}
