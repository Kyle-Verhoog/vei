package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object Name {
  def fromParseTreeNode(nameNode: ParseTreeNode[Token]): Name = {
    nameNode.childrenTypes match {
      case List("simple_name") =>
        new Name(AST.getValue(nameNode.children.head))
      case List("qualified_name") =>
        new Name(parseQualifiedName(nameNode.children.head))
    }
  }

  def parseQualifiedName(qualifiedNameNode: ParseTreeNode[Token]): String = {
    parseName(qualifiedNameNode.children.head) + "." + AST.getValue(
      qualifiedNameNode.children.last)
  }

  def parseName(nameNode: ParseTreeNode[Token]): String = {
    nameNode.childrenTypes match {
      case List("simple_name") =>
        AST.getValue(nameNode.children.head)
      case List("qualified_name") =>
        parseQualifiedName(nameNode.children.head)
    }
  }
}

class Name(val name: String) extends AST {
  val partsOfName = name.split(".")
}
