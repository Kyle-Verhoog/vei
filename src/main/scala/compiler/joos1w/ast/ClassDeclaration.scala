package compiler.joos1w.ast

import compiler.joos1w.environment.environment.Signature
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token
import compiler.parser.Parser
import compiler.scanner.Token

import scala.collection.mutable

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
      case "modifiers" ::
            "CLASS" ::
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

  def getExtends: Option[String] = {
    if (children.nonEmpty && children.head.isInstanceOf[Name]) {
      return Option(children.head.asInstanceOf[Name].name)
    }
    None
  }

  def getInterfaces: Option[List[String]] = {
    if (children.nonEmpty) {
      val head = children.head
      // check if interfaces is heard, otherwise it has to be the second child (or no where)
      if (head.isInstanceOf[ASTList] && head
            .asInstanceOf[ASTList]
            .getFieldName
            .equals("interface_type_list")) {
        return Option(
          head
            .asInstanceOf[ASTList]
            .children
            .map(child => child.asInstanceOf[Name].name))
      } else if (children.length > 1 && children(1)
                   .isInstanceOf[ASTList] && children(1)
                   .asInstanceOf[ASTList]
                   .getFieldName
                   .equals("interface_type_list")) {
        return Option(
          children(1)
            .asInstanceOf[ASTList]
            .children
            .map(child => child.asInstanceOf[Name].name))
      }
    }
    None
  }

  def getSuperSet: List[String] = {
    val extendsList = getExtends
    val interfacesList = getInterfaces

    var superSet = List[String]()

    if (extendsList.isDefined) superSet = superSet :+ extendsList.get
    if (interfacesList.isDefined)
      superSet = List.concat(superSet, interfacesList.get)

    // TODO determine what to do wyth java.lang.object if set is empty
    superSet
  }

  def getClassBody: ASTList = {
    children.last.asInstanceOf[ASTList]
  }

  /*
  def getDeclareSet: Map[Signature, AST] = {
    val map = mutable.Map[Signature, AST]()
    getClassBody.children
      .foreach(child =>
        child match {
          case c: MethodDeclaration => map += c.signature -> c
          case c: FieldDeclaration  => map += (c.name, List()) -> c
          case _                    =>
      })
    map.toMap
  }
  */

  def getImports: List[ImportDeclaration] = {
    val imports = parent.get.parent.get
      .asInstanceOf[CompilationUnit]
      .importDeclarations

    if (imports.isEmpty) return List()
    imports.get.getImports
  }

  override def strFields: String = {
    s"${modifiers.mkString(" ")} $identifier"
  }
}
