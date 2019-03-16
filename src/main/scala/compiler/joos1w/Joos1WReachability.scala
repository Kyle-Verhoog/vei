package compiler.joos1w
import compiler.joos1w.ast._

final case class ReachableException(
    private val message: String = "Code not reachable",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Joos1WReachability {
  def evalBooleanExpression(expr: AST): Boolean = {
    false
  }

  def reachable(ast: AST, isReachable: Boolean = true): Unit = {
    ast match {
      case ast: ClassDeclaration =>
      case ast: IfStatement =>
        val exprVal = evalBooleanExpression(ast.expr)
      case ast: ForStatement =>
        val exprVal = evalBooleanExpression(ast.termination)
      case ast: WhileStatement =>
        val exprVal = evalBooleanExpression(ast.expr)
        if (!exprVal) {
          throw ReachableException("While loop expression evaluated to false")
        }
      case _ => throw ReachableException("Undefined reachability")
    }
  }

  def checkReachability(ast: AST): Unit = {}
}
