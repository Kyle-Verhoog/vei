package compiler.ast

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

class MethodDeclaration() extends AST {
  def returnType: String = {
    this.getDescendant(1) match {
      case Some(n: MethodHeader) => n.returnType
      case e =>
        throw MalformedASTException(
          s"Method leftChild is not a MethodHeader (got $e).")
    }
  }

  def body: MethodBody = {
    this.getDescendant(1, Some(1)).get.asInstanceOf[MethodBody]
  }

  def modifiers: List[String] = {
    this.getDescendant(1) match {
      case Some(n: MethodHeader) => n.modifiers
      case e =>
        throw MalformedASTException(
          s"Method does not have MethodDeclarator child (got $e).")
    }
  }

  def identifier: String = {
    this.getDescendant(2, Some(1)) match {
      case Some(n: MethodDeclarator) => n.identifier
      case e =>
        throw MalformedASTException(
          s"Method does not have MethodDeclarator child (got $e.")
    }
  }

  override def strFields: String = {
    val mods = modifiers.mkString(" ")
    s"$mods $returnType $identifier"
  }

}
