package compiler.joos1w.ast

import compiler.parser.Parser
import compiler.scanner.Token

object TypeDeclaration extends ASTConstructor {
  def fromParseTree(parseTree: Parser.ParseTreeNode[Token],
                    parent: AST): AST = {
    val children = parseTree.children.toList
    val childrenTypes = parseTree.childrenTypes
    childrenTypes match {
      case "class_declaration" :: Nil | "interface_declaration" :: Nil =>
        AST.fromParseTreeAttachChildren(new TypeDeclaration, children)
      case ";" :: Nil | Nil => new Empty
      case _ =>
        throw ASTConstructionException()
    }
  }
}

class TypeDeclaration extends AST {}
