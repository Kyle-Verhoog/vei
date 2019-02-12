package compiler.joos1w.ast

import compiler.joos1w.ast.literals._
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

final case class MalformedASTException(
    private val message: String = "Malformed AST error.",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object AST {
  def getNthSibling(node: AST, sibNum: Integer): Option[AST] = {
    var sib = Option(node)
    (0 until sibNum).foreach(_ => {
      sib match {
        case Some(n: AST) => sib = n.rightSibling
        case None         => sib = None
      }
    })
    sib
  }

  def getNthChild(node: AST, childNum: Integer): Option[AST] = {
    val child = node.leftChild.get
    getNthSibling(child, childNum)
  }

  def getNthDescendant(node: AST, depth: Integer): Option[AST] = {
    var desc = Option(node)
    (0 until depth).foreach(_ => {
      desc match {
        case Some(n: AST) => desc = n.leftChild
        case None         => desc = None
      }
    })
    desc
  }

  /**
    * Gets a descendant node in the following structure:
    * (0, None) node
    * (1, None)/(1, 0) child0   (1, 1) child1   (1, 2) child2
    */
  def getDescendant(node: AST,
                    depth: Integer,
                    childNum: Option[Integer] = None): Option[AST] = {
    val desc = AST.getNthDescendant(node, depth)

    childNum match {
      case Some(i) => getNthSibling(desc.get, i)
      case None    => desc
    }
  }

  // TODO this method will return null if it gets converted to nothing (the empty branch cases)
  def convertParseTree(parseTree: ParseTreeNode[Token],
                       parent: Option[AST] = None): AST = {
    val children = parseTree.children
    /*
      Yes, I know nulls are discouraged, but using this will cause the compiler to crash
      if something goes wrong (which is probably a good thing) and took way less work
      than making this method return an opitonal
     */
    var ast: AST = null

    // build new node
    parseTree.token.tokenType match {
      case "compilation_unit" =>
        ast = new CompilationUnit(parseTree.token.value)

        parseTree.childrenTypes match {
          case List("package_declaration",
                    "import_declarations",
                    "type_declaration") =>
            recurseOnChildren(parseTree, ast)
        }
      case "type_declaration" =>
        ast = new TypeDeclaration

        parseTree.childrenTypes match {
          case List("class_declaration") | List("interface_declaration") =>
            recurseOnChildren(parseTree, ast)
          case List(";") | List() =>
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
      case "super" =>
        parseTree.childrenTypes match {
          case List("EXTENDS", "class_type") =>
            recurseOnChildren(parseTree, parent.get, List(1))
          case List() =>
        }
      case "field_declaration" =>
        parseTree.childrenTypes match {
          case List("modifiers", "type", "variable_declarator", ";") =>
            ast = FieldDeclaration.fromParseTreeNode(children.head)
            recurseOnChildren(parseTree, ast, List(1, 2))
        }
      case "variable_declarator" =>
        ast = new VariableDeclarator(getValue(children.head))
        parseTree.childrenTypes match {
          case List("variable_declarator_id") =>
          case List("IDENTIFIER")             =>
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
      case "single_type_import_declaration" =>
        parseTree.childrenTypes match {
          case List("IMPORT", "name", ";") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "type_import_on_demand_declaration" =>
        parseTree.childrenTypes match {
          case List("IMPORT", "name", ".", "*", ";") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "array_type" =>
        parseTree.childrenTypes match {
          case List("primitive_type", "[", "]") | List("name", "[", "]") =>
            recurseOnChildren(parseTree, parent.get, List(0))
        }
      case "name" =>
        parseTree.childrenTypes match {
          case List("simple_name") =>
            ast = new Name(getValue(children.head))
          case List("qualified_name") =>
            recurseOnChildren(parseTree, parent.get, List(0))
        }
      case "local_variable_declaration_statement" =>
        parseTree.childrenTypes match {
          case List("local_variable_declaration", ";") =>
            recurseOnChildren(parseTree, parent.get, List(0))
        }
      case "qualified_name" =>
        parseTree.childrenTypes match {
          case List("name", ".", "IDENTIFIER") =>
            ast = new Name(getValue(children(2)))
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "import_declarations" =>
        parseTree.childrenTypes match {
          case List("import_declarations", "import_declaration") =>
            recurseOnChildren(parseTree, parent.get, List(1))
          case List() =>
        }
      case "package_declaration" =>
        parseTree.childrenTypes match {
          case List("PACKAGE", "name", ";") =>
            ast = new PackageDeclaration()
            recurseOnChildren(parseTree, ast, List(1))
          case List() =>
        }
      case "interfaces" =>
        parseTree.childrenTypes match {
          case List("IMPLEMENTS", "interface_type_list") =>
            recurseOnChildren(parseTree, parent.get, List(1))
          case List() =>
        }
      case "interface_type_list" =>
        parseTree.childrenTypes match {
          case List("interface_type") =>
            recurseOnChildren(parseTree, parent.get, List(0))
          case List("interface_type_list", ",", "interface_type") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "class_body" =>
        parseTree.childrenTypes match {
          case List("{", "class_body_declarations", "}") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "class_body_declarations" =>
        parseTree.childrenTypes match {
          case List("class_body_declarations", "class_body_declaration") =>
            recurseOnChildren(parseTree, parent.get, List(0, 1))
          case List() =>
        }
      case "method_declaration" =>
        parseTree.childrenTypes match {
          case List("method_header", "method_body") =>
            ast = MethodDeclaration.fromParseTreeNode()
            recurseOnChildren(parseTree, ast, List(0, 1))
        }
      case "method_body" =>
        parseTree.childrenTypes match {
          case List("block") =>
            ast = new MethodBody(true)
            recurseOnChildren(parseTree, ast, List(0))
          case List(";") =>
            ast = new MethodBody(false)
        }
      case "method_header" =>
        parseTree.childrenTypes match {
          case List("modifiers", "type", "method_declarator") =>
            ast = MethodHeader.fromParseTreeNode(children.head)
            recurseOnChildren(parseTree, ast, List(1, 2))
          case List("modifiers", "VOID", "method_declarator") =>
            ast = MethodHeader.fromParseTreeNode(children.head)
            // add VOID type
            ast.addChildToEnd(Type.fromString(getValue(children(1))))
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
            recurseOnChildren(parseTree, parent.get, List(0, 2))
          case List("formal_parameter") =>
            recurseOnChildren(parseTree, parent.get, List(0))
          case List() =>
        }
      case "formal_parameter" =>
        parseTree.childrenTypes match {
          case List("type", "variable_declarator_id") =>
            ast = FormalParameter.fromParseTreeNode(parseTree)
            recurseOnChildren(parseTree, parent.get, List(0))
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
            ast = ConstructorDeclarator.fromParseTreeNode(children.head)
            recurseOnChildren(parseTree, ast, List(2))
        }
      case "interface_declaration" =>
        parseTree.childrenTypes match {
          case List("modifiers",
                    "INTERFACE",
                    "IDENTIFIER",
                    "extends_interfaces",
                    "interface_body") =>
            ast =
              InterfaceDeclaration.fromParseTreeNode(children.head, children(2))
            recurseOnChildren(parseTree, ast, List(3, 4))
        }
      case "extends_interfaces" =>
        parseTree.childrenTypes match {
          case List("EXTENDS", "interface_type") =>
            recurseOnChildren(parseTree, parent.get, List(1))
          case List("extends_interfaces", ",", "interface_type") =>
            recurseOnChildren(parseTree, parent.get, List(0, 2))
          case List() =>
        }
      case "interface_body" =>
        parseTree.childrenTypes match {
          case List("{", "interface_member_declarations", "}") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "interface_member_declarations" =>
        parseTree.childrenTypes match {
          case List("interface_member_declarations",
                    "interface_member_declaration") =>
            recurseOnChildren(parseTree, parent.get, List(0, 1))
          case List() =>
        }
      case "abstract_method_declaration" =>
        parseTree.childrenTypes match {
          case List("method_header", ";") =>
            ast = new AbstractMethodDeclaration()
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "block" =>
        parseTree.childrenTypes match {
          case List("{", "block_statements", "}") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "return_statement" =>
        parseTree.childrenTypes match {
          case List("RETURN", "expression_opt", ";") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "block_statements" =>
        parseTree.childrenTypes match {
          case List("block_statements", "block_statement") =>
            recurseOnChildren(parseTree, parent.get, List(0, 1))
          case List() =>
        }
      case "local_variable_declaration" =>
        parseTree.childrenTypes match {
          case List("type", "variable_declarator") =>
            ast = new LocalVariableDeclaration()
            recurseOnChildren(parseTree, ast)
        }
      case "type" =>
        // create and return the type (this will be a leaf)
        ast = Type.fromParseTreeNode(parseTree)
      case "expression_statement" =>
        parseTree.childrenTypes match {
          case List("statement_expression", ";") =>
            recurseOnChildren(parseTree, parent.get, List(0))
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
            recurseOnChildren(parseTree, parent.get, List(2, 4, 6))
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
      case "for_statement_no_short_if" =>
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
      case "primary_no_new_array" =>
        parseTree.childrenTypes match {
          case List("literal") | List("THIS") | List(
                "class_instance_creation_expression") | List("field_access") |
              List("method_invocation") | List("array") | List(
                "array_access") =>
            recurseOnChildren(parseTree, parent.get)
          case List("(", "expression", ")") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "literal" =>
        parseTree.childrenTypes match {
          // TODO verify
          // perform cast, this should handle lots of things without us checking anywhere else :)
          case List("INTEGER_LITERAL") =>
            ast = new IntegerLiteral(getValue(children.head))
          case List("BOOLEAN_LITERAL") =>
            ast = new BooleanLiteral(getValue(children.head).toBoolean)
          case List("CHARACTER_LITERAL") =>
            ast = new CharacterLiteral(getValue(children.head))
          case List("STRING_LITERAL") =>
            ast = new StringLiteral(getValue(children.head))
          case List("NULL_LITERAL") =>
            ast = new NullLiteral(getValue(children.head))
        }
      case "class_instance_creation_expression" =>
        parseTree.childrenTypes match {
          case List("NEW", "class_type", "(", "argument_list", ")") =>
            ast = new ClassInstanceCreation()
            recurseOnChildren(parseTree, parent.get, List(1, 3))
        }
      case "argument_list" =>
        parseTree.childrenTypes match {
          case List() =>
          case List("argument_list", ",", "expression") =>
            ast = new ArgumentList()
            recurseOnChildren(parseTree, ast, List(0, 2))
          case List("expression") =>
            ast = new ArgumentList()
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "array_creation_expression" =>
        parseTree.childrenTypes match {
          case List("NEW", "primitive_type", "dim_exprs") |
              List("NEW", "class_or_interface_type", "dim_exprs") =>
            ast = new ArrayCreationExpression(getValue(children(1)))
            recurseOnChildren(parseTree, ast, List(2))
        }
      case "primitive_type" =>
        ast = new PrimitiveType(getValue(children.head))
      case "dim_exprs" =>
        parseTree.childrenTypes match {
          case List("[", "expression", "]") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "dims" => {}
        // TODO verify this is what we want
        // Do nothing for dims
      case "field_access" =>
        parseTree.childrenTypes match {
          case List("primary", ".", "IDENTIFIER") =>
            ast = new FieldAccess(getValue(children(2)))
            recurseOnChildren(parseTree, ast, List(0))
        }
      case "method_invocation" =>
        parseTree.childrenTypes match {
          case List("name", "(", "argument_list", ")") =>
            ast = new MethodInvocation()
            recurseOnChildren(parseTree, ast, List(0, 2))
          case List("primary", ".", "IDENTIFIER", "(", "argument_list", ")") =>
            ast = new MethodInvocation(Some(getValue(children(2))))
            recurseOnChildren(parseTree, ast, List(0, 4))
        }
      case "array_access" =>
        parseTree.childrenTypes match {
          case List("name", "[", "expression", "]") =>
            ast = new ArrayAccess(Some(getValue(children(0))))
            recurseOnChildren(parseTree, ast, List(2))
          case List("primary_no_new_array", "[", "expression", "]") =>
            ast = new ArrayAccess(None)
            recurseOnChildren(parseTree, ast, List(0, 2))
        }
      case "conditional_expression" =>
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
      // TODO maybe case match like the rest, but im lazy now :)
      case "conditional_or_expression" | "conditional_and_expression" |
          "inclusive_or_expression" | "exclusive_or_expression" |
          "and_expression" | "equality_expression" | "relational_expression" |
          "shift_expression" | "additive_expression" |
          "multiplicative_expression" =>
        // TODO evaluate this approach
        if (children.isEmpty) {
          // TODO nothing to do? should never happen, throw?
          throw new RuntimeException("what is going on ???")
        } else if (children.length == 1) {
          recurseOnChildren(parseTree, parent.get)
        } else if (children.length == 2) {
          ast = new GeneralExpression(Some(getValue(children(0))))
          recurseOnChildren(parseTree, ast, List(1))
        } else if (children.length == 3) {
          ast = new GeneralExpression(Some(getValue(children(1))))
          recurseOnChildren(parseTree, ast, List(0, 2))
        } else {
          throw new RuntimeException("Too many children for an expression")
        }
      case "unary_expression" =>
        // need to handle unary_expression - unary_expression where the second unary
        // immediately goes to an integer literal, because in that case we must negate
        // the integer literal to properly check integer ranges
        parseTree.childrenTypes match {
          case List("-", "unary_expression") =>
            ast = new GeneralExpression(Some(getValue(children(0))))
            recurseOnChildren(parseTree, ast, List(1))
            // check if unary expression was IntegerLiteral, if so negate it
            ast.getChild(0).get match {
              case ast: IntegerLiteral =>
                // if an integer literal isnt a direct expression, check its bounds before negating
                if (!isDirectExpression(parseTree.children(1))) {
                  ast.integerValue // will trigger evaluation
                }
                ast.setNegative(true)
              case _ =>
            }
          case List("unary_expression_not_plus_minus") =>
            recurseOnChildren(parseTree, parent.get)
        }
      case "unary_expression_not_plus_minus" =>
        parseTree.childrenTypes match {
          case List("postfix_expression") | List("cast_expression") =>
            recurseOnChildren(parseTree, parent.get)
          case List("~", "unary_expression") |
              List("!", "unary_expression") =>
            recurseOnChildren(parseTree, parent.get, List(1))
        }
      case "postfix_expression" =>
        parseTree.childrenTypes match {
          case List("primary") | List("name") =>
            recurseOnChildren(parseTree, parent.get)
        }
      case "identifier" =>
        ast = new Identifier(parseTree.token.value)
      case "cast_expression" =>
        parseTree.childrenTypes match {
          case List("(", "primitive_type", ")", "unary_expression") |
              List("(", "expression", ")", "unary_expression_not_plus_minus") =>
            ast = CastExpression.fromParseTreeNode(parseTree)
            recurseOnChildren(parseTree, ast, List(1, 3))
          case List("(", "primitive_type", "dims", ")", "unary_expression") |
              List("(",
                   "name",
                   "dims",
                   ")",
                   "unary_expression_not_plus_minus") =>
            ast = CastExpression.fromParseTreeNode(parseTree)
            recurseOnChildren(parseTree, ast, List(1, 2, 4))
        }
      // TODO
      case _ =>
        children.length match {
          case 0 =>
          case 1 =>
            ast = convertParseTree(parseTree.children.head, parent)
          case _ =>
            throw new RuntimeException(
              "Too many children to process, make a rule for this type: " + parseTree.token.tokenType)
        }
    }

    ast
  }

  def isDirectExpression(parseTreeNode: ParseTreeNode[Token]): Boolean = {
    if (parseTreeNode.children.length > 1) return false
    if (parseTreeNode.children.nonEmpty)
      return isDirectExpression(parseTreeNode.children.head)
    true
  }

  // gets value recursively, will throw error if not a direct path
  def getValue(parseTreeNode: ParseTreeNode[Token]): String = {
    if (parseTreeNode.children.isEmpty) {
      if (parseTreeNode.token.value == "non-leaf") return null
      return parseTreeNode.token.value
    } else if (parseTreeNode.children.length == 1) {
      return getValue(parseTreeNode.children.head)
    }

    throw new RuntimeException(
      "Too many children to get value " + parseTreeNode.token)
  }

  // gets value recursively, will throw error if too many children
  def getValueList(parseTreeNode: ParseTreeNode[Token]): List[String] = {
    if (parseTreeNode.children.isEmpty) {
      if (parseTreeNode.token.value == "non-leaf") return List()
      return List(parseTreeNode.token.value)
    } else if (parseTreeNode.children.length == 1) {
      return getValueList(parseTreeNode.children.head)
    } else if (parseTreeNode.children.length == 2) {
      return List.concat(getValueList(parseTreeNode.children.head),
                         getValueList(parseTreeNode.children(1)))
    }

    throw new RuntimeException(
      "Too many children to get value list" + parseTreeNode.token)
  }

  def recurseOnChildren(parentParsTree: ParseTreeNode[Token],
                        parentAST: AST): Unit = {
    parentParsTree.children.foreach(child => {
      val childAST = convertParseTree(child, Some(parentAST))
      if (childAST != null) parentAST.addChildToEnd(childAST)
    })
  }

  def recurseOnChildren(parentParsTree: ParseTreeNode[Token],
                        parentAST: AST,
                        children: List[Int]): Unit = {
    children.foreach(i => {
      val childAST =
        convertParseTree(parentParsTree.children(i), Some(parentAST))
      if (childAST != null) parentAST.addChildToEnd(childAST)
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
      case Some(node) => node.addSiblingToEnd(sib)
      case None =>
        sib.parent = parent
        rightSibling = Some(sib)
    }
  }

  // add child to end of children list
  def addChildToEnd(newChild: AST): Unit = {
    leftChild match {
      case Some(node) => node.addSiblingToEnd(newChild)
      case None =>
        newChild.parent = Some(this)
        leftChild = Some(newChild)
    }
  }

  override def toString: String = {
    s"$strClass($strFields)"
  }

  def strClass: String = {
    getClass.toString.substring("class compiler.joos1w.ast.".length)
  }

  def strFields: String = {
    ""
  }

  def toStrTree: String = {
    val cs = children
      .map(
        child => {
          val childStrs = child.toStrTree.split("\n")
          val tailChar = if (childStrs.tail.isEmpty) "" else "\n"
          s"┠─ " + childStrs.head + tailChar + childStrs.tail
            .map(
              line => "┃  " + line
            )
            .mkString("\n")
        }
      )
      .mkString("\n")
    s"$toString\n$cs"
  }

  def nicelyFormattedTree(depth: Int = 0): String = {
    var line = ""
    for (_ <- 0 until depth) {
      line += "\t"
    }
    line += getClass
    line += "\n"
    if (leftChild.isDefined)
      line += leftChild.get.nicelyFormattedTree(depth + 1)
    if (rightSibling.isDefined)
      line += rightSibling.get.nicelyFormattedTree(depth)
    line
  }

  def siblings: List[AST] = {
    rightSibling match {
      case Some(n) => n :: n.siblings
      case None    => Nil
    }
  }

  def children: List[AST] = {
    leftChild match {
      case Some(n) => n :: n.siblings
      case None    => List[AST]()
    }
  }

  def getChild(n: Integer): Option[AST] = {
    AST.getNthChild(this, n)
  }

  def getDescendant(depth: Integer,
                    childNum: Option[Integer] = None): Option[AST] = {
    AST.getDescendant(this, depth, childNum)
  }
}
