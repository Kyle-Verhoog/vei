package compiler.ast

import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token
import jdk.jshell.spi.ExecutionControl.NotImplementedException

object AST {
  def convertParseTree(parseTree: ParseTreeNode[Token],
                       parent: Option[AST] = None): AST = {
    val children = parseTree.children
    var ast: AST = new AST()

    // build new node
    parseTree.token.tokenType match {
      case "compilation_unit" => {
        ast = new CompilationUnit

        parseTree.childrenTypes match {
          case List("package_declaration",
                    "import_declarations",
                    "type_declaration") =>
            recurseOnChildren(parseTree, ast)
        }
      }
      case "type_declaration" => {
        ast = new TypeDeclaration

        parseTree.childrenTypes match {
          case List("class_declaration") | List("interface_declaration") =>
            recurseOnChildren(parseTree, ast)
          case List(";") | List() =>
        }
      }
      case "class_declaration" =>
        parseTree.childrenTypes match {
          case List("modifiers",
                    "CLASS",
                    "IDENTIFIER",
                    "super",
                    "interfaces",
                    "class_body") =>
            ast = ClassDeclaration.fromParseTreeNode(children.head, children(2))
            recurseOnChildren(parseTree, ast, List(3, 4, 5))
        }
      case "field_declaration" =>
        parseTree.childrenTypes match {
          case List("modifiers", "type", "variable_declarator", ";") =>
            ast = FieldDeclaration.fromParseTreeNode(children.head, children(1))
            recurseOnChildren(parseTree, ast, List(2))
        }
      case "variable_declarator" =>
        ast = new VariableDeclarator(getValue(children.head))
        parseTree.childrenTypes match {
          case List("variable_declarator_id") =>
          case List("IDENTIFIER") =>
            recurseOnChildren(parseTree, ast, List(0))
          case List("variable_declarator_id", "=", "variable_initializer") =>
            recurseOnChildren(parseTree, ast, List(2))
        }
      case "assignment" =>
        ast = new Assignment()
        parseTree.childrenTypes match {
          case List("left_hand_side",
                    "assignment_operator",
                    "assignment_expression") =>
            recurseOnChildren(parseTree, ast, List(0, 2))
        }
      case "method_declaration" =>
        parseTree.childrenTypes match {
          case List("method_header", "method_body") =>
            ast = MethodDeclaration.fromParseTreeNode()
            recurseOnChildren(parseTree, ast, List(1))
        }
      case "method_header" =>
        parseTree.childrenTypes match {
          case List("modifiers", "type", "method_declarator") |
              List("modifiers", "VOID", "method_declarator") =>
            ast = MethodHeader.fromParseTreeNode(children.head, children(1))
            recurseOnChildren(parseTree, ast, List(2))
        }
      case "method_declarator" =>
        parseTree.childrenTypes match {
          case List("IDENTIFIER", "(", "formal_parameter_list", ")") =>
            ast = MethodDeclarator.fromParseTreeNode(children.head)
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "formal_parameter_list" =>
        parseTree.childrenTypes match {
          case List("formal_parameter_list", ",", "formal_parameter") =>
            recurseOnChildren(parseTree, ast, List(0, 2))
          case List("formal_parameter") =>
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "formal_parameter" =>
        parseTree.childrenTypes match {
          case List("type", "variable_declarator_id") =>
            ast = FormalParameter.fromParseTreeNode(children.head)
            recurseOnChildren(parseTree, ast, List(1, 2))
        }
      case "constructor_declaration" =>
        parseTree.childrenTypes match {
          case List("modifiers", "constructor_declarator", "block") =>
            ast = ConstructorDeclaration.fromParseTreeNode(children.head)
            recurseOnChildren(parseTree, ast, List(1, 2))
        }
      case "constructor_declarator" =>
        parseTree.childrenTypes match {
          case List("simple_name", "(", "formal_parameter_list", ")") =>
            ast = new ConstructorDeclarator()
            recurseOnChildren(parseTree, ast, List(0, 2))
        }
      case "interface_declaration" =>
        parseTree.childrenTypes match {
          case List("modifiers",
                    "INTERFACE",
                    "IDENTIFIER",
                    "extends_interface",
                    "interface_body") =>
            ast =
              InterfaceDeclaration.fromParseTreeNode(children.head, children(2))
            recurseOnChildren(parseTree, ast, List(3, 4))
        }
      case "extends_interfaces" =>
        parseTree.childrenTypes match {
          case List("EXTENDS", "interface_type") =>
            throw new NotImplementedException("extends interface")
          case List("extends_interfaces", ",", "interface_type") =>
            throw new NotImplementedException("extends interface")
        }
      case "interface_body" =>
        parseTree.childrenTypes match {
          case List("{", "interface_member_declarations", "}") =>
            recurseOnChildren(parseTree, ast, List(1))
        }
      case "interface_member_declarations" =>
        parseTree.childrenTypes match {
          case List("interface_member_declarations",
                    "interface_member_declaration") =>
            recurseOnChildren(parseTree, ast, List(0, 1))
          case List() =>
        }
      case "abstract_method_declaration" =>
        parseTree.childrenTypes match {
          case List("method_header", ";") =>
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "block" =>
        parseTree.childrenTypes match {
          case List("{", "block_statements", "}") =>
            recurseOnChildren(parseTree, ast, List(0, 1))
        }
      case "block_statements" =>
        parseTree.childrenTypes match {
          case List("block_statements", "block_statement") =>
            recurseOnChildren(parseTree, ast, List(0, 1))
          case List() =>
        }
      case "local_variable_declaration" =>
        parseTree.childrenTypes match {
          case List("type", "variable_declarator") =>
            ast = new LocalVariableDeclaration(getValue(children.head))
            recurseOnChildren(parseTree, ast, List(1))
        }
      case "expression_statement" =>
        parseTree.childrenTypes match {
          case List("statement_expression", ";") =>
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "if_then_statement" =>
        parseTree.childrenTypes match {
          case List("IF", "(", "expression", ")", "statement") =>
            recurseOnChildren(parseTree, ast, List(2, 4))
        }
      case "if_then_else_statement" =>
        parseTree.childrenTypes match {
          case List("IF",
                    "(",
                    "expression",
                    ")",
                    "statement_no_short_if",
                    "ELSE",
                    "statement") =>
            recurseOnChildren(parseTree, ast, List(2, 4, 6))
        }
      case "if_then_else_statement_no_short_if" =>
        parseTree.childrenTypes match {
          case List("IF",
                    "(",
                    "expression",
                    ")",
                    "statement_no_short_if",
                    "ELSE",
                    "statement_no_short_if") =>
            recurseOnChildren(parseTree, ast, List(2, 4, 6))
        }
      case "for_statement" =>
        parseTree.childrenTypes match {
          case List("FOR",
                    "(",
                    "for_init",
                    ";",
                    "expression_opt",
                    ";",
                    "for_update",
                    ")",
                    "statement_no_short_if") | List("FOR",
                                                    "(",
                                                    "for_init",
                                                    ";",
                                                    "expression_opt",
                                                    ";",
                                                    "for_update",
                                                    ")",
                                                    "statement") =>
            recurseOnChildren(parseTree, ast, List(2, 4, 6, 8))
        }
      case "while_statement" =>
        parseTree.childrenTypes match {
          case List("WHILE", "(", "expression", ")", "statement") =>
            recurseOnChildren(parseTree, ast, List(2, 4))
        }
      case "while_statement_no_short_if" =>
        parseTree.childrenTypes match {
          case List("WHILE", "(", "expression", ")", "statement_no_short_if") =>
            recurseOnChildren(parseTree, ast, List(2, 4))
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
      return getValue(parseTreeNode.children.head)
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

  def throws(): Unit = {
    throw new RuntimeException("Unmatched children!")
  }
}

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

  override def toString: String = {
    s"""\t$getClass $rightSibling
        \t$leftChild
       """.stripMargin
  }
}
