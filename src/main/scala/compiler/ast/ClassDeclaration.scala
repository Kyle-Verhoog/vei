package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object ClassDeclaration {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token],
                        identifier: ParseTreeNode[Token]): ClassDeclaration = {
    new ClassDeclaration(modifiers = AST.getValueList(modifiers),
                         identifier = AST.getValue(identifier))
  }
}

class ClassDeclaration(val modifiers: List[String], val identifier: String)
    extends AST {
  if (!(modifiers.contains("public") || modifiers.contains("private") || modifiers.contains("protected"))) {
    throw SemanticException("Methods must not be package private (eg. need public/private/protected)")
  }

  if (modifiers.contains("abstract") && modifiers.contains("final")) {
    throw SemanticException("A class cannot be both 'abstract' and 'final'")
  }
}
