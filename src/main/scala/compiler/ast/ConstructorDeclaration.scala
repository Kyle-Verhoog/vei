package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object ConstructorDeclaration {
  def fromParseTreeNode(
      modifiers: ParseTreeNode[Token]): ConstructorDeclaration = {
    println(AST.getValueList(modifiers))
    new ConstructorDeclaration(
      modifiers = AST.getValueList(modifiers)
    )
  }
}

class ConstructorDeclaration(modifiers: List[String]) extends AST {
  MethodHeader.validateModifiers(modifiers)
  //println("modifiers " + modifiers)
  if (modifiers.contains("abstract")) {
    throw SemanticException("Cannot have abstract constructors!")
  }
}
