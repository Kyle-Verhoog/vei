package compiler.joos1w.ast

import compiler.joos1w.environment.environment.Signature
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token
import compiler.parser.Parser
import compiler.scanner.Token

import scala.collection.immutable.Stream.Empty
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

  def getInterfaces: List[String] = {
    children(1) match {
      case ast: Name => List(ast.name)
      case ast: ASTList =>
        ast.children.map(child => child.asInstanceOf[Name].name)
      case ast: Empty => List()
    }
  }

  def getSuperSet: List[String] = {
    val extendsList = getExtends
    val interfacesList = getInterfaces

    var superSet = List[String]()

    if (extendsList.isDefined) superSet = superSet :+ extendsList.get
    if (superSet.isEmpty) superSet = List("java.lang.Object")

    superSet = List.concat(superSet, interfacesList)

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

  def hasZeroArgConstructor: Boolean = {
    constructors
      .exists(constructor => constructor.parameters.isEmpty)
  }

  def constructors: List[ConstructorDeclaration] = {
    getClassBody.children
      .filter(child => child.isInstanceOf[ConstructorDeclaration])
      .asInstanceOf[List[ConstructorDeclaration]]
  }

  override def strFields: String = {
    s"${modifiers.mkString(" ")} $identifier"
  }
}
