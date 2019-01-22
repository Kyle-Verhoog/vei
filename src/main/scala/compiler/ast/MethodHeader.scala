package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object MethodHeader {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token],
                        ttype: ParseTreeNode[Token]): MethodHeader = {
    new MethodHeader(
      modifiers = AST.getValueList(modifiers),
      returnType = AST.getValue(ttype)
    )
  }
}

class MethodHeader(val modifiers: List[String], val returnType: String)
    extends AST {
  // method validation
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