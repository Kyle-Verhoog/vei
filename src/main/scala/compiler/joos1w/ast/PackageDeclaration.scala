package compiler.joos1w.ast
import compiler.parser.Parser
import compiler.scanner.Token

object PackageDeclaration extends ASTConstructor {
  def fromParseTree(
      parseTree: Parser.ParseTreeNode[Token],
      parent: AST
  ): AST = {
    val children = parseTree.children.toList
    val childrenTypes = parseTree.childrenTypes
    childrenTypes match {
      case "package_declaration" :: "import_declarations" :: "type_declaration" :: Nil =>
        AST.fromParseTreeAttachChildren(new PackageDeclaration, children)
      case Nil => new Empty
      case _ =>
        throw ASTConstructionException()
    }
  }
}

class PackageDeclaration() extends AST {
  def name: String = {
    leftChild match {
      case Some(n: AST) =>
        n match {
          case n: Name => n.name
          case _       => throw new RuntimeException()
        }
      case _ => throw new RuntimeException()
    }
  }
}
