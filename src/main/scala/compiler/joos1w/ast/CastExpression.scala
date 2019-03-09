package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object CastExpression {
  def fromParseTreeNode(parseTreeNode: ParseTreeNode[Token]): CastExpression = {
    parseTreeNode.childrenTypes match {
      case List("(", "expression", ")", "unary_expression_not_plus_minus") =>
        verifyExpression(parseTreeNode.children(1))
        new CastExpression()
      case List("(", "primitive_type", ")", "unary_expression") |
          List("(", "primitive_type", "dims", ")", "unary_expression") |
          List("(", "name", "dims", ")", "unary_expression_not_plus_minus") =>
        new CastExpression()
    }
  }

  // recurse down and verify expression is appropriate for a cast
  def verifyExpression(node: ParseTreeNode[Token]): Unit = {
    if (node.children.length > 1) {
      throw SemanticException("Bad expression for cast, too many children")
    }

    if (node.token.tokenType == "postfix_expression") {
      if (node.childrenTypes == List("primary")) {
        throw SemanticException(
          "Illegal cast, cast should not have an expression that evaluates to primary"
        )
      }
      return // done since anything from here is ok
    }

    verifyExpression(node.children.head)
  }
}

class CastExpression extends AST {
  def simpleType: Option[String] = {
    children.head match {
      case child: PrimitiveType => {
        if (children.length == 2) {
          return Some(child.typeName)
        }
        Some(child.typeName + "[]")
      }
      case child: Name => {
        if (children.length == 2) {
          return Some(child.name)
        }
        Some(child.name + "[]")
      }
      case _ => None
    }
  }
}
