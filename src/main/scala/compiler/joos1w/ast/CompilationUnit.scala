package compiler.joos1w.ast
import compiler.parser.Parser
import compiler.scanner.Token

object CompilationUnit extends ASTConstructor {
  def fromParseTree(
      parseTree: Parser.ParseTreeNode[Token],
      parent: AST
  ): AST = {
    val children = parseTree.children.toList
    val childrenTypes = parseTree.childrenTypes
    childrenTypes match {
      case "package_declaration" :: "import_declarations" :: "type_declaration" :: Nil =>
        // val fileName = parseTree.token.value
        AST.fromParseTreeAttachChildren(
          new CompilationUnit(parseTree.token.value),
          children
        )
      case _ =>
        throw ASTConstructionException()
    }
  }
}

class CompilationUnit(name: String) extends AST {
  def fileName: String = {
    name.split("/").last.split("\\.java$").head.mkString
  }

  def packageDeclaration: Option[PackageDeclaration] = {
    children(0) match {
      case ast: PackageDeclaration => Option(ast)
      case ast: Empty => None
    }
  }

  def importDeclarations: Option[ImportDeclarationsList] = {
    children(1) match {
      case ast: ImportDeclarationsList => Option(ast)
      case ast: Empty => None
    }
  }

  def typeDeclaration: Option[AST] = {
    Option(children.last)
  }

  override def strFields: String = {
    s"$fileName.java"
  }
}
