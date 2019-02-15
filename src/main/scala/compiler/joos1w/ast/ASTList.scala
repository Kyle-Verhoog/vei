package compiler.joos1w.ast

class ASTList(fieldName: String = "") extends AST {
  override def strFields: String = {
    s"$fieldName"
  }
}
