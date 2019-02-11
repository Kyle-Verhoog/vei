package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object Type {
  def fromString(typeString: String): Type = {
    new Type(typeString)
  }

  def fromParseTreeNode(typeNode: ParseTreeNode[Token]): Type = {
    typeNode.childrenTypes match {
      case List("primitive_type") =>
        new Type(AST.getValue(typeNode.children.head))
      case List("reference_type") =>
        new Type(parseReferenceType(typeNode.children.head))
    }
  }

  def parseReferenceType(referenceTypeNode: ParseTreeNode[Token]): String = {
    referenceTypeNode.childrenTypes match {
      case List("class_or_interface_type") =>
        val classOrInstanceTypeNode = referenceTypeNode.children.head
        val nameNode = classOrInstanceTypeNode.children.head
        Name.parseName(nameNode) // returning the name
      case List("array_type") => {
        val arrayTypeNode = referenceTypeNode.children.head
        arrayTypeNode.childrenTypes match {
          case List("primitive_type", "[", "]") =>
            AST.getValue(arrayTypeNode.children.head) + "[]" // returning the name with []
          case List("name", "[", "]") =>
            Name.parseName(arrayTypeNode.children.head) + "[]" // returning the name with []
        }
      }

    }
  }
}

class Type(val ttype: String) extends AST {
  override def strFields: String = {
    s"$ttype"
  }
}
