package compiler.joos1w.ast

class ASTList(fieldName: String = "") extends AST {
  def length: Int = {
    children.length
  }

  override def strFields: String = {
    s"$fieldName ($length)"
  }
}
