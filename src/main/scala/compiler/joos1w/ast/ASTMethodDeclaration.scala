package compiler.joos1w.ast

import compiler.joos1w.environment.environment.Signature

abstract class ASTMethodDeclaration extends AST {
  def signature: Signature

  def returnType: String = {
    this.getDescendant(1) match {
      case Some(n: MethodHeader) => n.returnType
      case e =>
        throw MalformedASTException(
          s"Method leftChild is not a MethodHeader (got $e)."
        )
    }
  }

  // TODO: this return type can just be made MethodHeader since we
  //       throw an exception if it isn't present.
  def header: Option[MethodHeader] = {
    this.getDescendant(1, Some(0)) match {
      case Some(header: MethodHeader) => Some(header)
      case _ =>
        throw MalformedASTException(
          s"Expected method header, actual: " + this.getDescendant(1, Some(0)))
    }
  }

  def body: MethodBody = {
    this.getDescendant(1, Some(1)) match {
      case Some(body: MethodBody) => body
      case Some(_: Empty)         => new MethodBody(false)
      case _ =>
        throw MalformedASTException(
          s"Expected body got: " + this.getDescendant(1, Some(1)))
    }
  }

  def modifiers: List[String] = {
    this.getDescendant(1) match {
      case Some(n: MethodHeader) => n.modifiers
      case e =>
        throw MalformedASTException(
          s"Method does not have MethodDeclarator child (got $e)."
        )
    }
  }

  def identifier: String = {
    this.getDescendant(2, Some(1)) match {
      case Some(n: MethodDeclarator) => n.identifier
      case e =>
        throw MalformedASTException(
          s"Method does not have MethodDeclarator child (got $e)."
        )
    }
  }

  override def strFields: String = {
    val mods = modifiers.mkString(" ")
    val args =
      if (header.isDefined)
        header.get.methodDeclarator.parameters.children.fold("")((acc, c) =>
          acc + c.asInstanceOf[FormalParameter].ttype)
      else ""
    s"$mods $returnType $identifier $args"
  }
}
