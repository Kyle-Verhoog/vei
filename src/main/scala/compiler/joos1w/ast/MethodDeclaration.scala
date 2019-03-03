package compiler.joos1w.ast

import compiler.joos1w.environment.environment.Signature

object MethodDeclaration {

  def fromParseTreeNode(
  /*parseTree: ParseTreeNode[Token]*/ ): MethodDeclaration = {
    // children.childrenTypes match {
    //   case List("method_header", "method_body") =>
    //     val methodHeaderPTN = parseTree.children.head
    //     methodHeaderPTN.childrenTypes match {
    //       case List("IDENTIFIER", "(", "formal_parameter_list", ")") =>
    //         val ast = new MethodDeclaration(
    //           types = List[String](),
    //           identifier = "",
    //         )
    //         // add children
    //         val parameterList = AST.convertParseTree(methodHeaderPTN.children(2), Some(ast))
    //         ast
    //     }

    //     val methodIdentifier = AST.getValue()
    //     val methodBody = AST.convertParseTree(parseTree.children(1))
    //   case _ => throw new RuntimeException("Invalid method")
    // }
    new MethodDeclaration()
  }
}

class MethodDeclaration() extends ASTMethodDeclaration {
  def signature: Signature = {
    if (header.isDefined) return header.get.signature
    (identifier, Some(List()))
  }
}
