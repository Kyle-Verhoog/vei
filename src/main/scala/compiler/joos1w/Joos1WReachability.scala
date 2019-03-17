package compiler.joos1w
import compiler.joos1w.ast._
import compiler.joos1w.environment.types._

final case class ReachableException(
    private val message: String = "Code not reachable",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Joos1WReachability {
  abstract class Reachable
  case class Yes() extends Reachable
  case class No() extends Reachable
  case class Maybe() extends Reachable

  def out(ast: AST): Reachable = {
    ast match {
      case ast: Return => No()
      case ast: IfStatement =>
        println(ast.toStrTree)
        Maybe()
      case ast: MethodDeclaration => No()
      case _                      => Maybe()
    }
  }

  def evalNumericExpression(expr: AST): Option[Int] = {
    expr match {
      case expr: ConditionalExpression => None
      case expr: MethodInvocation      => None
      case expr: Name                  => None
    }
  }

  def evalGeneralExpression(expr: AST): Any = {
    expr match {
      case expr: ConditionalExpression   => None
      case expr: MethodInvocation        => None
      case expr: Name                    => None
      case expr: literals.IntegerLiteral => Some(expr.integerValue)
      case expr: literals.BooleanLiteral => Some(expr.value)
    }
  }

  def evalConstantExpression(expr: AST): Option[Boolean] = {
    println(s"EVALUATING EXPR ${expr.toStrTree}")
    expr match {
      case expr: ConditionalExpression =>
        val l = evalGeneralExpression(expr.firstExpr)
        val r = evalGeneralExpression(expr.secondExpr)
        (l, expr.operator, r) match {
          case (Some(i: Int), "==", Some(j: Int))         => Some(i == j)
          case (Some(_: Int), "==", None)                 => None
          case (None, "==", Some(_: Int))                 => None
          case (Some(i: Boolean), "||", Some(j: Boolean)) => Some(i || j)
          case (Some(_: Boolean), "||", None)             => None
          case (None, "||", Some(_: Boolean))             => None
          case (None, "&&", None)                         => None
        }
      case expr: Name => None
      /*
        expr.operator match {
          case "&&" =>
            val l = evalConstantExpression(expr.firstExpr)
            val r = evalConstantExpression(expr.secondExpr)
            (l, r) match {
              case (Some(i), Some(j)) => Some(i && j)
              case _                  => None
            }
          case "||" =>
            val l = evalConstantExpression(expr.firstExpr)
            val r = evalConstantExpression(expr.secondExpr)
            (l, r) match {
              case (Some(i), Some(j)) => Some(i || j)
              case _                  => None
            }
          // case "||" =>
          case "<" =>
            val l = evalNumericExpression(expr.firstExpr)
            val r = evalNumericExpression(expr.secondExpr)
            (l, r) match {
              case (Some(i), Some(j)) => Some(i < j)
              case _                  => None
            }
          // case ">" =>
          // case ">=" =>
          // case "<=" =>
          case "==" =>
            val l = evalGeneralExpression(expr.firstExpr)
            val r = evalGeneralExpression(expr.secondExpr)
            (l, r) match {
              // case (Some(i: Int), Some(j: Int)) =>
              case (Some(i: Int), Some(j: Int)) => Some(i == j)
            }
        }
       */
      case _ => throw new RuntimeException(s"$expr")
    }
  }

  def statementReachableCheck(ast: Option[AST],
                              reachable: Reachable): Reachable = {
    println(s"STATEMENT: AST: $ast REACHABLE: $reachable")
    if (reachable == No()) {
      throw ReachableException(s"Statement $ast not reachable")
    }

    ast match {
      case Some(ast: ForStatement) =>
        // evalBooleanExpression(ast.termination) match {
        //   case Some(result) =>
        //   case None =>
        // }
        Maybe()
      case Some(ast: WhileStatement) =>
        // L: while(E) S
        evalConstantExpression(ast.expr) match {
          // E = true: Infinite loop
          // in[S] = in[L]
          // out[L] = no
          case Some(true) =>
            statementReachableCheck(Some(ast.body), reachable)
            No()
          // E = false
          // in[S] = no
          // out[L] = in[L]
          case Some(false) =>
            statementReachableCheck(Some(ast.body), No())
            reachable
          // E
          // in[S] = in[L]
          // out[L] = in[L]
          case None =>
            statementReachableCheck(Some(ast.body), reachable)
            reachable
        }
      case Some(ast: TopLevelIf) =>
        // val exprVal = evalBooleanExpression(ast.expr)
        val s =
          ast.children.map(childIf =>
            statementReachableCheck(Some(childIf), reachable))
        val out = s.foldLeft[Reachable](Maybe()) {
          case (acc: Maybe, reachable: Maybe) => Maybe()
          case (acc: No, reachable: Maybe)    => Maybe()
          case (acc: Maybe, reachable: No)    => Maybe()
          case (acc: No, reachable: No)       => No()
          case (acc, reachable) =>
            throw new RuntimeException(s"acc: $acc reachable: $reachable")
        }
        out
      case Some(ast: Return) =>
        No()
      case Some(ast: AST) => reachable
    }
  }

  def reachableCheck(ast: Option[AST], reachable: Reachable): Reachable = {
    println(s"AST: $ast REACHABLE: $reachable")
    ast match {
      case Some(ast: CompilationUnit) =>
        reachableCheck(ast.typeDeclaration, reachable)
      case Some(ast: TypeDeclaration) =>
        reachableCheck(ast.leftChild, reachable)
      case Some(ast: ClassDeclaration) =>
        reachableCheck(ast.getChild(2), reachable)
      case Some(ast: ASTList) =>
        ast.fieldName match {
          case "class_body_declarations" =>
            ast.children.foreach(child =>
              reachableCheck(Some(child), reachable))
            Maybe()
          case "block_statements" =>
            // var out: Reachable = reachable
            // ast.children.foreach(ast => out = reachableCheck(Some(ast), out))
            // out
            ast.children.foldLeft[Reachable](reachable) {
              case (out, next) => statementReachableCheck(Some(next), out)
            }
        }
      case Some(ast: InterfaceDeclaration) => Maybe()
      case Some(ast: MethodDeclaration) =>
        reachableCheck(Some(ast.body), Maybe())
      case Some(ast: ConstructorDeclaration) =>
        reachableCheck(Some(ast.children(1)), Maybe())
      case Some(ast: MethodBody) =>
        reachableCheck(ast.leftChild, Maybe())
      case Some(ast: FieldDeclaration) => Maybe()
      case Some(ast: Empty)            => reachable
      case None                        => reachable
      case _                           => throw ReachableException(s"Undefined reachability $ast")
    }
  }

  def checkReachability(ast: AST): Unit = {
    reachableCheck(Some(ast), Maybe())
  }

  def checkReachability(asts: List[AST]): Unit = {
    asts.foreach(ast => checkReachability(ast))
  }
}
