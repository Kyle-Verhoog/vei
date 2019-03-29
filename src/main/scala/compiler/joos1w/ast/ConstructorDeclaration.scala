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

  /*
    ------ NOTE ------
    HACK: Constructors have a special signature where the first argument type
    is the type of the class itself! yes this has bad side effects if someone
    makes a method with the same signature... but relaly this wont happen :)

   */
  def signature: (String, Option[List[String]]) = {
    if (header.isDefined) return header.get.signature
    (identifier.split('.').last,
     Some(
       List(identifier) ++ constructorDeclarator.parameters.children.map(
         child => child.asInstanceOf[FormalParameter].ttype.split('.').last)))
  }

  def constructorDeclarator: ConstructorDeclarator = {
    children.head.asInstanceOf[ConstructorDeclarator]
  }

  def rawParameters: List[AST] = {
    constructorDeclarator.parameters.children
  }

  def parameters: List[String] = {
    constructorDeclarator.parameters.children.map(child =>
      child.asInstanceOf[FormalParameter].ttype.split('.').last)
  }

  override def header: Option[MethodHeader] = {
    None
  }

  def hasBody: Boolean = {
    children.last.isInstanceOf[Empty]
  }

  override def body: MethodBody = {
    val hasBody = children.last.isInstanceOf[Empty]
    val methodBody = new MethodBody(hasBody)
    children.last.children.foreach(child => methodBody.addChildToEnd(child))
    methodBody
  }

  override def returnType: String = "void"
}
