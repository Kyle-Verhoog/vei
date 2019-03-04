package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object ConstructorDeclaration {
  def fromParseTreeNode(
      modifiers: ParseTreeNode[Token]
  ): ConstructorDeclaration = {
    new ConstructorDeclaration(
      mods = AST.getValueList(modifiers)
    )
  }
}

class ConstructorDeclaration(mods: List[String]) extends ASTMethodDeclaration {
  MethodHeader.validateModifiers(mods)
  //println("modifiers " + modifiers)
  if (mods.contains("abstract")) {
    throw SemanticException("Cannot have abstract constructors!")
  }

  override def identifier: String = {
    this.getChild(0) match {
      case Some(n: ConstructorDeclarator) => n.name
      case e =>
        throw MalformedASTException(
          s"ConsturctorDeclaration does not have ConstructorDeclarator child (got $e."
        )
    }
  }

  override def modifiers: List[String] = {
    mods
  }

  def signature: (String, Option[List[String]]) = {
    if (header.isDefined) return header.get.signature
    (identifier.split('.').last,
     Some(constructorDeclarator.parameters.children.map(child =>
       child.asInstanceOf[FormalParameter].ttype.split('.').last)))
  }

  def constructorDeclarator: ConstructorDeclarator = {
    children.head.asInstanceOf[ConstructorDeclarator]
  }

  override def header: Option[MethodHeader] = {
    None
  }

  override def body: MethodBody = {
    val hasBody = children.last.isInstanceOf[Empty]
    val methodBody = new MethodBody(hasBody)
    children.last.children.foreach(child => methodBody.addChildToEnd(child))
    methodBody
  }

  override def returnType: String = "void"
}
