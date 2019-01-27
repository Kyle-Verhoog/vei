package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object MethodHeader {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token]): MethodHeader = {
    new MethodHeader(
      modifiers = AST.getValueList(modifiers)
    )
  }

  def validateModifiers(modifiers: List[String]): Unit = {
    // method validation
    if (!(modifiers.contains("public") || modifiers.contains("private") || modifiers
          .contains("protected"))) {
      throw SemanticException(
        "Methods must not be package private (eg. need public/private/protected)")

    }
    if (modifiers.contains("private")) {
      throw SemanticException("Methods cannot be private")
    }
    if (modifiers.contains("abstract")) {
      if (modifiers.contains("static") || modifiers.contains("final")) {
        throw SemanticException(
          "An abstract method cannot be 'static' or 'final'.")
      }
    }
    if (modifiers.contains("static")) {
      if (modifiers.contains("final")) {
        throw SemanticException("A static method cannot be 'final'.")
      }
    }
  }
}

class MethodHeader(val modifiers: List[String]) extends AST {
  MethodHeader.validateModifiers(modifiers)

  def returnType: String = {
    getChild(0).asInstanceOf[Type].ttype
  }
}
