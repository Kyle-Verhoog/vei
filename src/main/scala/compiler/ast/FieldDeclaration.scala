package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object FieldDeclaration {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token],
                        ttype: ParseTreeNode[Token]): FieldDeclaration = {
    new FieldDeclaration(
      modifiers = AST.getValueList(modifiers),
      fieldType = AST.getValue(ttype)
    )
  }
}

// TODO determine how we want to handle modifiers, type and stuff
class FieldDeclaration(modifiers: List[String], fieldType: String) extends AST {
  if (!(modifiers.contains("public") || modifiers.contains("private") || modifiers.contains("protected"))) {
    throw SemanticException("Methods must not be package private (eg. need public/private/protected)")
  }
  if (modifiers.contains("final")) {
    throw SemanticException("No field can be 'final'.")
  }
  if (modifiers.contains("private")) {
    throw SemanticException("No field can be 'private'.")
  }
}
