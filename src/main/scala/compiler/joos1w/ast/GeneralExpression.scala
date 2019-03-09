package compiler.joos1w.ast

class GeneralExpression(val operation: Option[String]) extends AST {

  def firstExpr: AST = {
    children.head
  }

  def secondExpr: AST = {
    children(1)
  }

  override def strFields: String = {
    operation match {
      case Some(s) => s"$s"
      case None    => ""
    }
  }
}
