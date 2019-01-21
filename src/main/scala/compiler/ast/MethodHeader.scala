package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object MethodHeader {
  def fromParseTreeNode(modifiers: ParseTreeNode[Token],
                        ttype: ParseTreeNode[Token]): MethodHeader = {
    // if (ttype.token.tokenType == "VOID") {
    //   ttype =
    // }

    new MethodHeader(
      modifiers = AST.getValueList(modifiers),
      ttype = AST.getValue(ttype)
    )
  }
}

class MethodHeader(modifiers: List[String], ttype: String) extends AST {}
