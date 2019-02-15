package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object Type {
  def fromString(typeString: String): Type = {
    new Type(typeString)
  }

  def fromParseTreeNode(typeNode: ParseTreeNode[Token]): Type = {
    typeNode.childrenTypes match {
      case "primitive_type" :: Nil =>
        new Type(AST.getValue(typeNode.children.head))
      case "reference_type" :: Nil =>
        new Type(parseReferenceType(typeNode.children.head))
      case _ =>
        throw ASTConstructionException(s"")
    }
  }

  def parseReferenceType(referenceTypeNode: ParseTreeNode[Token]): String = {
    referenceTypeNode.childrenTypes match {
      case "class_or_interface_type" :: Nil =>
        val classOrInstanceTypeNode = referenceTypeNode.children.head
        val nameNode = classOrInstanceTypeNode.children.head
        Name.parseName(nameNode) // returning the name
      case "array_type" :: Nil =>
        val arrayTypeNode = referenceTypeNode.children.head
        arrayTypeNode.childrenTypes match {
          case "primitive_type" :: "[" :: "]" :: Nil =>
            AST.getValue(arrayTypeNode.children.head) + "[]" // returning the name with []
          case "name" :: "[" :: "]" :: Nil =>
            Name.parseName(arrayTypeNode.children.head) + "[]" // returning the name with []
          case _ =>
            throw ASTConstructionException(s"")
        }
      case _ =>
        throw ASTConstructionException(s"")
    }
  }
}

class Type(val ttype: String) extends AST {
  override def strFields: String = {
    s"$ttype"
  }
}
