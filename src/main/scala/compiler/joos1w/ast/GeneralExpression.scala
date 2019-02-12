package compiler.joos1w.ast

class GeneralExpression(operation: Option[String]) extends AST {

  override def strFields: String = {
    operation match {
      case Some(s) => s"$s"
      case None    => ""
    }
  }
}
