package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

import compiler.parser.Parser
import compiler.scanner.Token

object ClassDeclaration extends ASTConstructor {

  def fromParseTreeNode(
      modifiers: ParseTreeNode[Token],
      identifier: ParseTreeNode[Token]
  ): ClassDeclaration = {
    new ClassDeclaration(
      modifiers = AST.getValueList(modifiers),
      identifier = AST.getValue(identifier)
    )
  }

  def fromParseTree(
      parseTree: Parser.ParseTreeNode[Token],
      parent: AST
  ): AST = {
    val children = parseTree.children.toList
    val childrenTypes = parseTree.childrenTypes
    childrenTypes match {
      case "modifiers" :: "CLASS" ::
            "IDENTIFIER" ::
            "super" ::
            "interfaces" ::
            "class_body" :: Nil =>
        val ast = new ClassDeclaration(
          AST.getValueList(children(0)),
          AST.getValue(children(2))
        )
        // recurse on "super" "interfaces" "class_body"
        // println("children: ", children.tail.tail.tail)
        AST.fromParseTreeAttachChildren(ast, children.tail.tail.tail)
      case _ =>
        throw ASTConstructionException()
    }
  }
}

class ClassDeclaration(val modifiers: List[String], val identifier: String)
    extends AST {
  if (!(modifiers.contains("public") || modifiers.contains("private") || modifiers
        .contains("protected"))) {
    throw SemanticException(
      "Methods must not be package private (eg. need public/private/protected)"
    )
  }

  if (modifiers.contains("abstract") && modifiers.contains("final")) {
    throw SemanticException("A class cannot be both 'abstract' and 'final'")
  }

  override def strFields: String = {
    s"${modifiers.mkString(" ")} $identifier"
  }
}
