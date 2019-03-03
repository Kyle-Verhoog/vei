package compiler.joos1w.ast

import compiler.joos1w.Joos1WCFG
import compiler.joos1w.ast.literals._
import compiler.parser.Parser.ParseTreeNode
import compiler.scanner.Token

final case class MalformedASTException(
    private val message: String = "Malformed AST error.",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

final case class ASTConstructionException(
    private val message: String = "AST construction error.",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

trait ASTConstructor {
  def fromParseTree(parseTree: ParseTreeNode[Token], parent: AST): AST
}

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

  def convertParseTree(value: ParseTreeNode[Token]): AST = {
    throw new RuntimeException("TODO replaced by kyle")
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
  def getDescendant(
      node: AST,
      depth: Integer,
      childNum: Option[Integer] = None
  ): Option[AST] = {
    val desc = AST.getNthDescendant(node, depth)

    childNum match {
      case Some(i) => getNthSibling(desc.get, i)
      case None    => desc
    }
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
      "Too many children to get value " + parseTreeNode.token
    )
  }

  // gets value recursively, will throw error if too many children
  def getValueList(parseTreeNode: ParseTreeNode[Token]): List[String] = {
    if (parseTreeNode.children.isEmpty) {
      if (parseTreeNode.token.value == "non-leaf") return List()
      return List(parseTreeNode.token.value)
    } else if (parseTreeNode.children.length == 1) {
      return getValueList(parseTreeNode.children.head)
    } else if (parseTreeNode.children.length == 2) {
      return List.concat(
        getValueList(parseTreeNode.children.head),
        getValueList(parseTreeNode.children(1))
      )
    }

    throw new RuntimeException(
      "Too many children to get value list" + parseTreeNode.token
    )
  }

  def fromParseTreeAttachASTChildren(parent: AST, children: List[AST]): AST = {
    children.foreach(child => {
      if (child == parent) {
        throw ASTConstructionException(s"Child $child equals parent $parent")
      }
      child.parent = Some(parent)
      parent.addChildToEnd(child)
    })
    parent
  }

  def fromParseTreeAttachChildren(
      parent: AST,
      children: List[ParseTreeNode[Token]]
  ): AST = {
    children.foreach(child => {
      val childAST = fromParseTree(child, parent)
      if (childAST == parent) {
        throw ASTConstructionException(
          s"Child $child $childAST equals parent $parent"
        )
      }

      val childASTLabelled = childAST match {
        case _: Empty => new Empty(child.token.tokenType)
        case _        => childAST
      }
      childASTLabelled.parent = Some(parent)
      parent.addChildToEnd(childASTLabelled)
    })
    parent
  }

  def fromParseTreeList(list: List[ParseTreeNode[Token]], parent: AST): AST = {
    list.length match {
      case 0 =>
        new Empty
      case 1 => // LIST
        val item = fromParseTree(list.head, parent)
        parent.addChildToEnd(item)
      case 2 => // LIST ITEM
        val item = fromParseTree(list(1), parent)
        fromParseTreeList(list.head.children.toList, parent)
        parent.addChildToEnd(item)
      case 3 => // LIST , ITEM
        val item = fromParseTree(list(2), parent)
        fromParseTreeList(list.head.children.toList, parent)
        parent.addChildToEnd(item)
      case _ =>
        throw ASTConstructionException(s"$parent $list")
    }
    parent
  }

  def fromParseTree(parseTree: ParseTreeNode[Token]): AST = {
    val root = new Empty
    val ast = fromParseTree(parseTree, root)
    ast
  }

  def fromParseTree(parseTree: ParseTreeNode[Token], parent: AST): AST = {
    val tokenType = parseTree.token.tokenType
    val children = parseTree.children.toList
    val childrenTypes = parseTree.childrenTypes

    tokenType match {
      case "compilation_unit" =>
        childrenTypes match {
          case "package_declaration" :: "import_declarations" :: "type_declaration" :: Nil =>
            fromParseTreeAttachChildren(
              new CompilationUnit(parseTree.token.value),
              children
            )
          case _ =>
            throw ASTConstructionException()
        }
      case "package_declaration" =>
        childrenTypes match {
          case "PACKAGE" :: "name" :: ";" :: Nil =>
            fromParseTreeAttachChildren(
              new PackageDeclaration,
              children(1) :: Nil
            )
          case Nil => new Empty
          case _ =>
            throw ASTConstructionException()
        }
      case "import_declarations" =>
        childrenTypes match {
          case "import_declarations" :: "import_declaration" :: Nil =>
            val l = new ImportDeclarationsList()
            fromParseTreeList(children, l)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "class_or_interface_type" =>
        childrenTypes match {
          case "name" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interface_type" =>
        childrenTypes match {
          case "class_or_interface_type" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interface_type_list" =>
        childrenTypes match {
          case "interface_type_list" :: "," :: "interface_type" :: Nil =>
            val l = new ASTList("interface_type_list")
            fromParseTreeList(children, l)
          case "interface_type" :: Nil =>
            fromParseTree(children.head, parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "import_declaration" =>
        childrenTypes match {
          case "single_type_import_declaration" :: Nil |
              "type_import_on_demand_declaration" :: Nil =>
            fromParseTreeAttachChildren(
              new ImportDeclaration(),
              children
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "single_type_import_declaration" =>
        childrenTypes match {
          case "IMPORT" :: "name" :: ";" :: Nil =>
            fromParseTree(children(1), parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "type_import_on_demand_declaration" =>
        childrenTypes match {
          case "IMPORT" :: "name" :: "." :: "*" :: ";" :: Nil =>
            // TODO: special node type?
            Name.fromImportStarParseTreeNode(children(1))
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "type_declaration" =>
        childrenTypes match {
          case "class_declaration" :: Nil | "interface_declaration" :: Nil =>
            fromParseTreeAttachChildren(new TypeDeclaration, children)
          case ";" :: Nil | Nil => new Empty
          case _                => throw ASTConstructionException()
        }
      case "class_declaration" =>
        childrenTypes match {
          case "modifiers" :: "CLASS" ::
                "IDENTIFIER" ::
                "super" ::
                "interfaces" ::
                "class_body" :: Nil =>
            // recurse on "super" "interfaces" "class_body"
            fromParseTreeAttachChildren(
              new ClassDeclaration(
                getValueList(children.head),
                getValue(children(2))
              ),
              children.tail.tail.tail
            )
          case _ => throw ASTConstructionException()
        }
      case "super" =>
        childrenTypes match {
          case "EXTENDS" :: "class_type" :: Nil =>
            fromParseTree(children(1), parent)
          case Nil => new Empty()
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "class_type" =>
        childrenTypes match {
          case "class_or_interface_type" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interfaces" =>
        childrenTypes match {
          case "IMPLEMENTS" :: "interface_type_list" :: Nil =>
            fromParseTree(children(1), parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "constructor_declaration" =>
        childrenTypes match {
          case "modifiers" :: "constructor_declarator" :: "block" :: Nil =>
            fromParseTreeAttachChildren(
              ConstructorDeclaration.fromParseTreeNode(children.head),
              children.tail
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "constructor_declarator" =>
        childrenTypes match {
          case "simple_name" :: "(" :: "formal_parameter_list" :: ")" :: Nil =>
            fromParseTreeAttachChildren(
              ConstructorDeclarator.fromParseTreeNode(children.head),
              children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interface_declaration" =>
        childrenTypes match {
          case "modifiers" ::
                "INTERFACE" ::
                "IDENTIFIER" ::
                "extends_interfaces" ::
                "interface_body" :: Nil =>
            fromParseTreeAttachChildren(
              InterfaceDeclaration
                .fromParseTreeNode(children.head, children(2)),
              children.tail.tail.tail
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interface_body" =>
        childrenTypes match {
          case "{" :: "interface_member_declarations" :: "}" :: Nil =>
            fromParseTree(children(1), parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interface_member_declarations" =>
        parseTree.childrenTypes match {
          case "interface_member_declarations" ::
                "interface_member_declaration" :: Nil =>
            val l = new ASTList("interface_member_declarations")
            fromParseTreeList(children, l)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "interface_member_declaration" =>
        childrenTypes match {
          case "abstract_method_declaration" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "extends_interfaces" =>
        childrenTypes match {
          case "extends_interfaces" :: "," :: "interface_type" :: Nil =>
            val l = new ASTList("extends_interfaces")
            fromParseTreeList(children, l)
          case "EXTENDS" :: "interface_type" :: Nil =>
            fromParseTree(children(1), parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "class_body" =>
        childrenTypes match {
          case "{" :: "class_body_declarations" :: "}" :: Nil =>
            fromParseTree(children(1), parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "class_body_declarations" =>
        childrenTypes match {
          case "class_body_declarations" :: "class_body_declaration" :: Nil =>
            val l = new ASTList("class_body_declarations")
            fromParseTreeList(children, l)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "class_body_declaration" =>
        childrenTypes match {
          case "class_member_declaration" :: Nil |
              "constructor_declaration" :: Nil =>
            fromParseTree(children.head, parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "class_member_declaration" =>
        childrenTypes match {
          case "field_declaration" :: Nil | "method_declaration" :: Nil =>
            fromParseTree(children.head, parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "method_declaration" =>
        childrenTypes match {
          case "method_header" :: "method_body" :: Nil =>
            fromParseTreeAttachChildren(
              MethodDeclaration.fromParseTreeNode(),
              children
            )
          case _ =>
            throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "method_header" =>
        childrenTypes match {
          case "modifiers" :: "type" :: "method_declarator" :: Nil =>
            fromParseTreeAttachChildren(
              MethodHeader.fromParseTreeNode(children.head),
              children.tail
            )
          case "modifiers" :: "VOID" :: "method_declarator" :: Nil =>
            val ast = MethodHeader.fromParseTreeNode(children.head)
            // TODO add VOID type
            ast.addChildToEnd(Type.fromString(getValue(children(1))))
            fromParseTreeAttachChildren(
              ast,
              children.tail.tail
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "method_body" =>
        childrenTypes match {
          case "block" :: Nil =>
            fromParseTreeAttachChildren(
              new MethodBody(true),
              children
            )
          case ";" :: Nil => new Empty
          case _          => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "method_declarator" =>
        childrenTypes match {
          case "IDENTIFIER" :: "(" :: "formal_parameter_list" :: ")" :: Nil =>
            fromParseTreeAttachChildren(
              MethodDeclarator.fromParseTreeNode(children.head),
              children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "formal_parameter_list" =>
        childrenTypes match {
          case "formal_parameter_list" :: "," :: "formal_parameter" :: Nil =>
            val p = new ASTList("formal_parameter_list")
            fromParseTreeList(children, p)
          case "formal_parameter" :: Nil =>
            fromParseTreeAttachChildren(
              new ASTList("formal_parameter_list"),
              children.head :: Nil
            )
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "formal_parameter" =>
        childrenTypes match {
          case "type" :: "variable_declarator_id" :: Nil =>
            fromParseTreeAttachChildren(
              new FormalParameter(getValue(children(1))),
              children.head :: Nil
            )
          case _ =>
            throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "type" =>
        // create and return the type (this will be a leaf)
        childrenTypes match {
          case "primitive_type" :: Nil | "reference_type" :: Nil =>
            Type.fromParseTreeNode(parseTree)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "block" =>
        childrenTypes match {
          case "{" :: "block_statements" :: "}" :: Nil =>
            fromParseTree(children(1), parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "block_statements" =>
        childrenTypes match {
          case "block_statements" :: "block_statement" :: Nil =>
            val p = new ASTList("block_statements")
            fromParseTreeList(children, p)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "block_statement" =>
        childrenTypes match {
          case "local_variable_declaration_statement" :: Nil |
              "statement" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "statement" =>
        childrenTypes match {
          case "statement_without_trailing_substatement" :: Nil |
              "if_then_statement" :: Nil | "if_then_else_statement" :: Nil |
              "while_statement" :: Nil | "for_statement" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "local_variable_declaration_statement" =>
        childrenTypes match {
          case "local_variable_declaration" :: ";" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "statement_without_trailing_substatement" =>
        childrenTypes match {
          case "block" :: Nil | "empty_statement" :: Nil |
              "expression_statement" :: Nil | "return_statement" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "expression_statement" =>
        childrenTypes match {
          case "statement_expression" :: ";" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "local_variable_declaration" =>
        childrenTypes match {
          case "type" :: "variable_declarator" :: Nil =>
            fromParseTreeAttachChildren(
              new LocalVariableDeclaration(),
              children
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "variable_declarator" =>
        childrenTypes match {
          case "variable_declarator_id" :: Nil =>
            new VariableDeclarator(getValue(children.head))
          case "variable_declarator_id" :: "=" :: "variable_initializer" :: Nil =>
            fromParseTreeAttachChildren(
              new VariableDeclarator(getValue(children.head)),
              children(2) :: Nil
            )
          case "IDENTIFIER" :: Nil =>
            new VariableDeclarator(getValue(children.head))
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "variable_initializer" =>
        childrenTypes match {
          case "expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "qualified_name" =>
        childrenTypes match {
          case "name" :: "." :: "IDENTIFIER" :: Nil =>
            fromParseTreeAttachChildren(
              new Name(getValue(children(2))),
              children.head :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "expression" =>
        childrenTypes match {
          case "assignment_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "assignment_expression" =>
        childrenTypes match {
          case "conditional_expression" :: Nil | "assignment" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "conditional_expression" =>
        childrenTypes match {
          case "conditional_or_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "conditional_or_expression" =>
        childrenTypes match {
          case "conditional_or_expression" :: "||" :: "conditional_and_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some("||")),
              children.head :: children(2) :: Nil
            )
          case "conditional_and_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "conditional_and_expression" =>
        childrenTypes match {
          case "conditional_and_expression" :: "&&" :: "inclusive_or_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some("&&")),
              children.head :: children(2) :: Nil
            )
          case "inclusive_or_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "inclusive_or_expression" =>
        childrenTypes match {
          case "exclusive_or_expression" :: "|" :: "exclusive_or_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some("|")),
              children.head :: children(2) :: Nil
            )
          case "exclusive_or_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "exclusive_or_expression" =>
        childrenTypes match {
          case "and_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "and_expression" =>
        childrenTypes match {
          case "and_expression" :: "&" :: "equality_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some("&")),
              children.head :: children(2) :: Nil
            )
          case "equality_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "equality_expression" =>
        childrenTypes match {
          case "relational_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case "equality_expression" :: _ :: "relational_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some(getValue(children(1)))),
              children.head :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "relational_expression" =>
        childrenTypes match {
          case "shift_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case "relational_expression" :: _ :: "additive_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some(getValue(children(1)))),
              children.head :: children(2) :: Nil
            )
          case "relational_expression" :: "INSTANCEOF" :: "reference_type" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some(getValue(children(1)))),
              children.head :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "shift_expression" =>
        childrenTypes match {
          case "additive_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "additive_expression" =>
        childrenTypes match {
          case "multiplicative_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case "additive_expression" :: _ :: "multiplicative_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some(getValue(children(1)))),
              children.head :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "multiplicative_expression" =>
        childrenTypes match {
          case "unary_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case "multiplicative_expression" :: _ :: "unary_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new GeneralExpression(Some(getValue(children(1)))),
              children.head :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "unary_expression" =>
        // need to handle unary_expression - unary_expression where the second unary
        // immediately goes to an integer literal, because in that case we must negate
        // the integer literal to properly check integer ranges
        childrenTypes match {
          case "-" :: "unary_expression" :: Nil =>
            val ast = fromParseTreeAttachChildren(
              new GeneralExpression(Some(getValue(children.head))),
              children(1) :: Nil
            )
            // check if unary expression was IntegerLiteral, if so negate it
            ast.getChild(0).get match {
              case ast: IntegerLiteral =>
                // if an integer literal isn't a direct expression, check its bounds before negating
                if (!isDirectExpression(parseTree.children(1))) {
                  ast.integerValue // will trigger evaluation
                }
                ast.setNegative(true)
                ast
              case _ => ast
            }
          case "unary_expression_not_plus_minus" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "unary_expression_not_plus_minus" =>
        childrenTypes match {
          case "cast_expression" :: Nil | "postfix_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case op :: "unary_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new UnaryExpression(op),
              children.tail
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "postfix_expression" =>
        childrenTypes match {
          case "name" :: Nil | "primary" :: Nil =>
            fromParseTree(children.head, parent)
          case "postfix_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "primary" =>
        childrenTypes match {
          case "primary_no_new_array" :: Nil =>
            fromParseTree(children.head, parent)
          case "array_creation_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "primary_no_new_array" =>
        childrenTypes match {
          case "literal" :: Nil | "method_invocation" :: Nil |
              "field_access" :: Nil | "array_access" :: Nil |
              "class_instance_creation_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case "THIS" :: Nil => new Empty
          case "(" :: "expression" :: ")" :: Nil =>
            fromParseTree(children(1), parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "literal" =>
        childrenTypes match {
          // TODO verify
          // perform cast, this should handle lots of things without us checking anywhere else :)
          case "INTEGER_LITERAL" :: Nil =>
            new IntegerLiteral(getValue(children.head))
          case "BOOLEAN_LITERAL" :: Nil =>
            new BooleanLiteral(getValue(children.head).toBoolean)
          case "CHARACTER_LITERAL" :: Nil =>
            new CharacterLiteral(getValue(children.head))
          case "STRING_LITERAL" :: Nil =>
            new StringLiteral(getValue(children.head))
          case "NULL_LITERAL" :: Nil =>
            new NullLiteral(getValue(children.head))
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "field_declaration" =>
        childrenTypes match {
          case "modifiers" :: "type" :: "variable_declarator" :: ";" :: Nil =>
            fromParseTreeAttachChildren(
              FieldDeclaration.fromParseTreeNode(children.head),
              children(1) :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "field_access" =>
        childrenTypes match {
          case "primary" :: "." :: "IDENTIFIER" :: Nil =>
            fromParseTreeAttachChildren(
              new FieldAccess(getValue(children(2))),
              children.head :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "statement_expression" =>
        childrenTypes match {
          case "assignment" :: Nil | "method_invocation" :: Nil |
              "class_instance_creation_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "assignment" =>
        childrenTypes match {
          case "left_hand_side" ::
                "assignment_operator" ::
                "assignment_expression" :: Nil =>
            fromParseTreeAttachChildren(
              new Assignment(),
              children.head :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "left_hand_side" =>
        childrenTypes match {
          case "name" :: Nil | "field_access" :: Nil | "array_access" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "name" =>
        childrenTypes match {
          case "simple_name" :: Nil =>
            Name.fromParseTreeNode(parseTree)
          case "qualified_name" :: Nil =>
            Name.fromParseTreeNode(parseTree)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "method_invocation" =>
        childrenTypes match {
          case "name" :: "(" :: "argument_list" :: ")" :: Nil =>
            fromParseTreeAttachChildren(
              new MethodInvocation(None),
              children(2) :: children.head :: Nil
            )
          case "primary" :: "." :: "IDENTIFIER" :: "(" :: "argument_list" :: ")" :: Nil =>
            fromParseTreeAttachChildren(
              new MethodInvocation(Some(getValue(children(2)))),
              children(4) :: children.head :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }

      case "argument_list" =>
        childrenTypes match {
          case "argument_list" :: "," :: "expression" :: Nil =>
            val p = new ASTList("argument_list")
            fromParseTreeList(children, p)
          case "expression" :: Nil =>
            fromParseTree(children.head, parent)
          // fromParseTreeAttachChildren(parent, children)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "array_creation_expression" =>
        childrenTypes match {
          case "NEW" :: "primitive_type" :: "dim_exprs" :: Nil |
              "NEW" :: "class_or_interface_type" :: "dim_exprs" :: Nil =>
            fromParseTreeAttachChildren(
              new ArrayCreationExpression(getValue(children(1))),
              children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "dim_exprs" =>
        childrenTypes match {
          case "[" :: "expression" :: "]" :: Nil =>
            fromParseTree(children(1), parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "array_access" =>
        childrenTypes match {
          case "name" :: "[" :: "expression" :: "]" :: Nil =>
            fromParseTreeAttachChildren(
              new ArrayAccess(),
              children.head :: children(2) :: Nil
            )
          case "primary_no_new_array" :: "[" :: "expression" :: "]" :: Nil =>
            fromParseTreeAttachChildren(
              new ArrayAccess(),
              children.head :: children(2) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "return_statement" =>
        childrenTypes match {
          case "RETURN" :: "expression_opt" :: ";" :: Nil =>
            fromParseTreeAttachChildren(
              new Return,
              children(1) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "expression_opt" =>
        childrenTypes match {
          case "expression" :: Nil =>
            fromParseTree(children.head, parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "reference_type" =>
        childrenTypes match {
          case "class_or_interface_type" :: Nil | "array_type" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "array_type" =>
        childrenTypes match {
          case "primitive_type" :: "[" :: "]" :: Nil |
              "name" :: "[" :: "]" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "abstract_method_declaration" =>
        childrenTypes match {
          case "method_header" :: ";" :: Nil =>
            fromParseTreeAttachChildren(
              new AbstractMethodDeclaration(),
              children.head :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "if_then_statement" =>
        childrenTypes match {
          case "IF" :: "(" :: "expression" :: ")" :: "statement" :: Nil =>
            fromParseTreeAttachChildren(
              new IfStatement(),
              children(2) :: children(4) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "if_then_else_statement" =>
        parseTree.childrenTypes match {
          case "IF" ::
                "(" ::
                "expression" ::
                ")" ::
                "statement_no_short_if" ::
                "ELSE" ::
                "statement" :: Nil =>
            fromParseTreeAttachChildren(
              new IfStatement(),
              children(2) :: children(4) :: children(6) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "statement_no_short_if" =>
        parseTree.childrenTypes match {
          case "statement_without_trailing_substatement" :: Nil |
              "if_then_else_statement_no_short_if" :: Nil |
              "while_statement_no_short_if" :: Nil |
              "for_statement_no_short_if" :: Nil =>
            fromParseTree(children.head, parent)
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "if_then_else_statement_no_short_if" =>
        childrenTypes match {
          case "IF" ::
                "(" ::
                "expression" ::
                ")" ::
                "statement_no_short_if" ::
                "ELSE" ::
                "statement_no_short_if" :: Nil =>
            fromParseTreeAttachChildren(
              new IfStatement(),
              children(2) :: children(4) :: children(6) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "for_init" =>
        childrenTypes match {
          case "local_variable_declaration" :: Nil |
              "statement_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "for_update" =>
        childrenTypes match {
          case "statement_expression" :: Nil =>
            fromParseTree(children.head, parent)
          case Nil => new Empty
          case _   => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "for_statement" =>
        childrenTypes match {
          case "FOR" :: "(" ::
                "for_init" :: ";" ::
                "expression_opt" :: ";" ::
                "for_update" :: ")" :: "statement" :: Nil =>
            fromParseTreeAttachChildren(
              new ForStatement(),
              children(2) :: children(4) :: children(6) :: children(8) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "for_statement_no_short_if" =>
        childrenTypes match {
          case "FOR" ::
                "(" ::
                "for_init" ::
                ";" ::
                "expression_opt" ::
                ";" ::
                "for_update" ::
                ")" ::
                "statement_no_short_if" :: Nil =>
            fromParseTreeAttachChildren(
              new ForStatement(),
              children(2) :: children(4) :: children(6) :: children(8) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "while_statement" =>
        childrenTypes match {
          case "WHILE" :: "(" :: "expression" :: ")" :: "statement" :: Nil =>
            fromParseTreeAttachChildren(
              new WhileStatement(),
              children(2) :: children(4) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "while_statement_no_short_if" =>
        childrenTypes match {
          case "WHILE" :: "(" :: "expression" :: ")" :: "statement_no_short_if" :: Nil =>
            fromParseTreeAttachChildren(
              new WhileStatement(),
              children(2) :: children(4) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "primitive_type" =>
        new PrimitiveType(getValue(children.head))
      case "class_instance_creation_expression" =>
        childrenTypes match {
          case "NEW" :: "class_type" :: "(" :: "argument_list" :: ")" :: Nil =>
            fromParseTreeAttachChildren(
              new ClassInstanceCreation(),
              children(1) :: children(3) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "cast_expression" =>
        childrenTypes match {
          case "(" :: "primitive_type" :: ")" :: "unary_expression" :: Nil |
              "(" :: "expression" :: ")" :: "unary_expression_not_plus_minus" :: Nil =>
            fromParseTreeAttachChildren(
              CastExpression.fromParseTreeNode(parseTree),
              children(1) :: children(3) :: Nil
            )
          case "(" :: "primitive_type" :: "dims" :: ")" :: "unary_expression" :: Nil |
              "(" ::
                "name" ::
                "dims" ::
                ")" ::
                "unary_expression_not_plus_minus" :: Nil =>
            fromParseTreeAttachChildren(
              CastExpression.fromParseTreeNode(parseTree),
              children(1) :: children(2) :: children(4) :: Nil
            )
          case _ => throw ASTConstructionException(s"$tokenType $childrenTypes")
        }
      case "dims" => new Empty
      case _ =>
        throw ASTConstructionException(s"Unhandled token type '$tokenType'")
    }
  }

  def getTerminalValues(parseTreeNode: ParseTreeNode[Token]): List[String] = {
    if (parseTreeNode.children.isEmpty) {
      if (parseTreeNode.token.value == "non-leaf") return List()
      return List(parseTreeNode.token.value)
    } else if (parseTreeNode.children.length == 1) {
      return getValueList(parseTreeNode.children.head)
    } else if (parseTreeNode.children.length == 2) {
      return List.concat(
        getValueList(parseTreeNode.children.head),
        getValueList(parseTreeNode.children(1))
      )
    }

    throw new RuntimeException(
      "Too many children to get value list" + parseTreeNode.token
    )
  }

  def throws(): Unit = {
    throw new RuntimeException("Unmatched children!")
  }
}

class AST(
    var parent: Option[AST] = None,
    var leftChild: Option[AST] = None,
    var rightSibling: Option[AST] = None
) {

  // adds sibling to end of sibling list and sets siblings parent accordingly
  def addSiblingToEnd(sib: AST): Unit = {
    rightSibling match {
      case Some(node) => node.addSiblingToEnd(sib)
      case None =>
        sib.parent = parent
        rightSibling = Some(sib)
    }
  }

  def fromParseTree(parseTree: ParseTreeNode[Token]): AST = {
    AST.convertParseTree(parseTree)
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

  def getDescendant(
      depth: Integer,
      childNum: Option[Integer] = None
  ): Option[AST] = {
    AST.getDescendant(this, depth, childNum)
  }
}
