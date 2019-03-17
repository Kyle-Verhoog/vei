package compiler.joos1w
import compiler.joos1w.ast._

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

  def in(ast: AST): Reachable = {
    ast match {
      case ast: IfStatement =>
        println(ast.toStrTree)
        Maybe()
      case _ => Maybe()
    }
  }

  def evalBooleanExpression(expr: AST): Boolean = {
    false
  }

  def statementReachableCheck(ast: Option[AST],
                              reachable: Reachable): Reachable = {
    println(s"STATEMENT: AST: $ast REACHABLE: $reachable")
    ast match {
      case Some(ast: ForStatement) =>
        val exprVal = evalBooleanExpression(ast.termination)
        No()
      case Some(ast: WhileStatement) =>
        val exprVal = evalBooleanExpression(ast.expr)
        if (!exprVal) {
          throw ReachableException("While loop expression evaluated to false")
        }
        Maybe()
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
        reachableCheck(ast.rightSibling, out)
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
