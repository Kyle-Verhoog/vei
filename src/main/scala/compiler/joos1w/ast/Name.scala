package compiler.joos1w.ast

import compiler.joos1w.environment.types.AbstractType
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

  // BIG HACK
  def fromImportStarParseTreeNode(nameNode: ParseTreeNode[Token]): Name = {
    nameNode.childrenTypes match {
      case List("simple_name") =>
        new Name(AST.getValue(nameNode.children.head) + ".*")
      case List("qualified_name") =>
        new Name(parseQualifiedName(nameNode.children.head) + ".*")
    }
  }

  def parseQualifiedName(qualifiedNameNode: ParseTreeNode[Token]): String = {
    parseName(qualifiedNameNode.children.head) + "." + AST.getValue(
      qualifiedNameNode.children.last
    )
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
  val partsOfName = name.split('.')

  // eg. for A.B.C() where A is a class, B a class, C a field
  // we have instance = "C" object = ""A.B""
  var instanceField: Option[String] = None
  var instanceType: Option[AbstractType] = None
  var objectPart: Option[String] = None
  var objectType: Option[AbstractType] = None
  var staticField: Option[String] = None
  var staticType: Option[AbstractType] = None

  override def strFields: String = {
    s"$name"
  }
}
