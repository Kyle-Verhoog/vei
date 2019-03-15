package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object FieldDeclaration {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token]): FieldDeclaration = {
    new FieldDeclaration(
      modifiers = AST.getValueList(modifiers)
    )
  }
}

// TODO determine how we want to handle modifiers, type and stuff
class FieldDeclaration(val modifiers: List[String]) extends AST {
  if (!(modifiers.contains("public") || modifiers.contains("private") || modifiers
        .contains("protected"))) {
    throw SemanticException(
      "Fields must not be package private (eg. need public/private/protected)"
    )
  }

  if (modifiers.contains("final")) {
    throw SemanticException("No field can be 'final'.")
  }
  if (modifiers.contains("private")) {
    throw SemanticException("No field can be 'private'.")
  }

  def fieldType: String = {
    getChild(0).get.asInstanceOf[Type].ttype
  }

  def name: String = {
    getChild(1).get.asInstanceOf[VariableDeclarator].name
  }

  def variableDeclarator: VariableDeclarator = {
    children.last match {
      case child: VariableDeclarator => child
    }
  }

  override def strFields: String = {
    s"$fieldType $name"
  }
}
