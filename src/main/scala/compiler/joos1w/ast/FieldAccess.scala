package compiler.joos1w.ast

class FieldAccess(name: String) extends AST {
  override def strFields: String = {
    s"$name"
  }
}
