package compiler.joos1w.ast

// TODO option, yea or nay?
class ArrayAccess extends AST {
  def name: Option[String] = {
    children.head match {
      case child: Name => Some(child.name)
      case _           => None
    }
  }

  def expression: AST = {
    children.last
  }

  def primary: AST = {
    children.head
  }

  override def strFields: String = {
    children.head match {
      case child: Name => child.name
      case _           => "Not direct access"
    }
  }
}
