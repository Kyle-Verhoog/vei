package compiler.joos1w.ast

class FieldAccess(name: String) extends AST {
  def primary: AST = {
    children.head
  }

  def identifier: String = {
    name
  }

  override def strFields: String = {
    s"$name"
  }
}
