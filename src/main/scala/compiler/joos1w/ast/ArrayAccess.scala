package compiler.joos1w.ast

// TODO option, yea or nay?
class ArrayAccess(name: Option[String]) extends AST {
  override def strFields: String = {
    name match {
      case Some(name: String) => s"$name"
      case _                  => ""
    }
  }
}
