package compiler.joos1w.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object InterfaceDeclaration {
  def fromParseTreeNode(
      modifiers: ParseTreeNode[Token],
      identifier: ParseTreeNode[Token]
  ): InterfaceDeclaration = {
    new InterfaceDeclaration(
      modifiers = AST.getValueList(modifiers),
      identifier = AST.getValue(identifier)
    )
  }
}

class InterfaceDeclaration(modifiers: List[String], val identifier: String)
    extends AST {
  def getExtends: List[String] = {
    children.head match {
      case ast: ASTList =>
        ast.children
          .map(child => child.asInstanceOf[Name].name)
      case _ => List()
    }
  }

  def getImports: List[ImportDeclaration] = {
    val imports = parent.get.parent.get
      .asInstanceOf[CompilationUnit]
      .importDeclarations

    if (imports.isEmpty) return List()
    imports.get.getImports
  }
}
