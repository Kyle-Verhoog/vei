package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

object AST {

  class AST(var parent: Option[AST] = None,
            var leftChild: Option[AST] = None,
            var rightSibling: Option[AST] = None) {

    // adds sibling to end of sibling list and sets siblings parent accordingly
    def addSiblingToEnd(sib: AST): Unit = {
      rightSibling match {
        case Some(node) => rightSibling.get.addSiblingToEnd(sib)
        case None => {
          sib.parent = parent
          rightSibling = Some(sib)
        }
      }
    }

    // add child to end of children list
    def addChildToEnd(newChild: AST): Unit = {
      leftChild match {
        case Some(node) => node.addSiblingToEnd(newChild)
        case None => {
          leftChild = Some(newChild)
        }
      }
    }

    override def toString = {
      s"""\t$getClass $rightSibling
        \t$leftChild
       """.stripMargin
    }
  }

  def convertParseTree(parseTree: ParseTreeNode[Token],
                       parent: Option[AST] = None): AST = {
    val children = parseTree.children
    var ast: AST = new AST()

    println(parseTree.token.tokenType)

    // build new node
    parseTree.token.tokenType match {
      case "compilation_unit" => {
        ast = new CompilationUnit
        recurseOnChildren(parseTree, ast)
      }
      case "type_declaration" => {
        ast = new TypeDeclaration
        recurseOnChildren(parseTree, ast)
      }
      case "class_declaration" => {
        ast = new ClassDeclaration(getValue(children(2)))
        recurseOnChildren(parseTree, ast, List(5))
      }
      case "field_declaration" => {
        ast = new FieldDeclaration(getValueList(children.head),
                                       getValue(children(1)))
        recurseOnChildren(parseTree, ast, List(2))
      }
      case "variable_declarator" => {
        ast = new VariableDeclarator(getValue(children.head))
        recurseOnChildren(parseTree, ast, List(2))
      }
      case "assignment" => {
        ast = new Assignment()
        recurseOnChildren(parseTree, ast, List(0,2))
      }
      case _ => {
        children.length match {
          case 0 => ast = new AST()
          case 1 => ast = convertParseTree(parseTree.children.head, parent)
          case _ => {
            ast = new AST()
            recurseOnChildren(parseTree, ast)
          }
        }
      }
    }

    ast
  }

  // gets value recursively, will throw error if not a direct path
  def getValue(parseTreeNode: ParseTreeNode[Token]): String = {
    if (parseTreeNode.children.isEmpty) {
      return parseTreeNode.token.value
    } else if (parseTreeNode.children.length == 1) {
      return getValue(parseTreeNode.children(0))
    }

    throw new RuntimeException("Too many children to get value")
  }

  // gets value recursively, will throw error if too many children
  def getValueList(parseTreeNode: ParseTreeNode[Token]): List[String] = {
    if (parseTreeNode.children.isEmpty) {
      return List(parseTreeNode.token.value)
    } else if (parseTreeNode.children.length == 1) {
      return getValueList(parseTreeNode.children.head)
    } else if (parseTreeNode.children.length == 2) {
      return List.concat(getValueList(parseTreeNode.children.head),
                         getValueList(parseTreeNode.children(1)))
    }

    throw new RuntimeException("Too many children to get value list")
  }

  def recurseOnChildren(parentParsTree: ParseTreeNode[Token],
                        parentAST: AST): Unit = {
    parentParsTree.children.foreach(child => {
      parentAST.addChildToEnd(
        convertParseTree(child, Some(parentAST))
      )
    })
  }

  def recurseOnChildren(parentParsTree: ParseTreeNode[Token],
                        parentAST: AST,
                        children: List[Int]): Unit = {
    children.foreach(i => {
      parentAST.addChildToEnd(
        convertParseTree(parentParsTree.children(i), Some(parentAST))
      )
    })
  }
}
