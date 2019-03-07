package compiler.joos1w.ast

class ASTList(val fieldName: String = "") extends AST {
  def length: Int = {
    children.length
  }

  def getFieldName: String = {
    fieldName
  }

  override def strFields: String = {
    s"$fieldName ($length)"
  }
}
