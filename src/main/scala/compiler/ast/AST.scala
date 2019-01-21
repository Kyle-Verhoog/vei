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
      case "import_declaration" =>
        parseTree.childrenTypes match {
          case List("single_type_import_declaration") | List(
                "type_import_on_demand_declaration") =>
            ast = new ImportDeclaration()
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "import_declarations" =>
        parseTree.childrenTypes match {
          case List("import_declarations", "import_declaration") =>
            recurseOnChildren(parseTree, ast, List(1))
          case List() =>
        }
      case "package_declaration" =>
        parseTree.childrenTypes match {
          case List("PACKAGE", "name", ";") =>
            ast = new PackageDeclaration()
            recurseOnChildren(parseTree, ast, List(1))
          case List() =>
        }
      case "interface_type_list" =>
        parseTree.childrenTypes match {
          case List("interface_type_list", ",", "interface_type") =>
            recurseOnChildren(parseTree, ast, List(1))
        }
      case "class_body" =>
        parseTree.childrenTypes match {
          case List("{", "class_body_declarations", "}") =>
            recurseOnChildren(parseTree, ast, List(1))
        }
      case "class_body_declarations" =>
        parseTree.childrenTypes match {
          case List("class_body_declarations", "class_body_declaration") =>
            recurseOnChildren(parseTree, ast, List(0, 1))
          case List() =>
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
            ast = new IfStatement()
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
            ast = new IfStatement()
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
            ast = new ForStatement()
            recurseOnChildren(parseTree, ast, List(2, 4, 6, 8))
        }
      case "while_statement" =>
        parseTree.childrenTypes match {
          case List("WHILE", "(", "expression", ")", "statement") =>
            ast = new WhileStatement()
            recurseOnChildren(parseTree, ast, List(2, 4))
        }
      case "while_statement_no_short_if" =>
        parseTree.childrenTypes match {
          case List("WHILE", "(", "expression", ")", "statement_no_short_if") =>
            ast = new WhileStatement()
            recurseOnChildren(parseTree, ast, List(2, 4))
        }
      case "primary_no_new_array" => {
        parseTree.childrenTypes match {
          case List("literal") | List("THIS") | List(
                "class_instance_creation_expression") | List("field_access") |
              List("method_invocation") | List("array") =>
            recurseOnChildren(parseTree, parent.get)
          case List("(", "expression", ")") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      }
      case "class_instance_creation_expression" => {
        parseTree.childrenTypes match {
          case List("NEW", "class_type", "(", "argument_list", ")") =>
            ast = new ClassInstanceCreation(getValue(children(1)))
            recurseOnChildren(parseTree, parent.get, List(3))
        }
      }
      case "argument_list" => {
        parseTree.childrenTypes match {
          case List() =>
          case List("argument_list", ",", "expression") =>
            ast = new ArgumentList()
            recurseOnChildren(parseTree, ast, List(0, 2))
          case List("expression") =>
            ast = new ArgumentList()
            recurseOnChildren(parseTree, ast, List(0))
        }
      }
      case "array_creation_expression" => {
        parseTree.childrenTypes match {
          case List("NEW", "primitive_type", "dim_exprs", "dims_opt") |
              List("NEW", "class_or_interface_type", "dim_exprs", "dims_opt") =>
            ast = new ArrayCreationExpression(getValue(children(1)))
            recurseOnChildren(parseTree, ast, List(2, 3))
        }
      }
      case "dim_exprs" => {
        // TODO figure out if multi dimensions are needed, if not simplify
        throw new RuntimeException("TODO")
      }
      case "dim_expr" => {
        // TODO figure out if multi dimensions are needed, if not simplify
        throw new RuntimeException("TODO")
      }
      case "dims" => {
        // TODO figure out if multi dimensions are needed, if not simplify
        throw new RuntimeException("TODO")
      }
      case "dims_opt" => {
        // TODO figure out if multi dimensions are needed, if not simplify
        throw new RuntimeException("TODO")
      }
      case "field_access" => {
        parseTree.childrenTypes match {
          case List("primary", ".", "IDENTIFIER") =>
            ast = new FieldAccess(getValue(children(2)))
            recurseOnChildren(parseTree, ast, List(0))
        }
      }
      case "method_invocation" => {
        parseTree.childrenTypes match {
          case List("name", "(", "argument_list", ")") =>
            ast = new MethodInvocation()
            recurseOnChildren(parseTree, ast, List(0))
          case List("primary", ".", "IDENTIFIER", "(", "argument_list", ")") =>
            ast = new MethodInvocation(Some(getValue(children(2))))
            recurseOnChildren(parseTree, ast, List(0, 4))
        }
      }
      case "array_access" => {
        parseTree.childrenTypes match {
          case List("name", "[", "expression", "]") =>
            ast = new ArrayAccess(Some(getValue(children(0))))
            recurseOnChildren(parseTree, ast, List(2))
          case List("primary_no_new_array", "[", "expression", "]") =>
            ast = new ArrayAccess(None)
            recurseOnChildren(parseTree, ast, List(0, 2))
        }
      }
      case "conditional_expression" => {
        parseTree.childrenTypes match {
          case List("conditional_or_expression") =>
            recurseOnChildren(parseTree, parent.get, List(0))
          case List("conditional_or_expression",
                    "?",
                    "expression",
                    ":",
                    "conditional_expression") =>
            ast = new ConditionalExpression()
            recurseOnChildren(parseTree, ast, List(2, 4))
        }
      }
      // TODO maybe case match like the rest, but im lazy now :)
      case "conditional_or_expression" | "conditional_and_expression" |
          "inclusive_or_expression" | "exclusive_or_expression" |
          "and_expression" | "equality_expression" | "relational_expression" |
          "shift_expression" | "additive_expression" |
          "multiplicative_expression" | "unary_expression" => {
        // TODO evaluate this approah
        if (children.isEmpty) {
          // TODO nothing to do? should never happen, throw?
          throw new RuntimeException("what is going on ???")
        } else if (children.length == 1) {
          recurseOnChildren(parseTree, parent.get, List(0))
        } else if (children.length == 2) {
          ast = new GeneralExpression(Some(getValue(children(0))))
          recurseOnChildren(parseTree, ast, List(1))
        } else if (children.length == 3) {
          ast = new GeneralExpression(Some(getValue(children(1))))
          recurseOnChildren(parseTree, ast, List(0, 2))
        }
      }
      case "unary_expression_not_plus_minus" => {
        parseTree.childrenTypes match {
          case List("postfix_expression") | List("cast_expression") =>
            recurseOnChildren(parseTree, parent.get)
          case List("~ unary_expression") | List("~ unary_expression") => {
            recurseOnChildren(parseTree, ast, List(1))
          }
        }
      }
      case "postfix_expression" => {
        parseTree.childrenTypes match {
          case List("primary") | List("name") =>
            recurseOnChildren(parseTree, parent.get)
        }
      }
      case "cast_expression" => {} // TODO
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
