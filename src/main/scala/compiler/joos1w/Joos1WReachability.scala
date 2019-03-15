package compiler.joos1w
import compiler.joos1w.ast.AST

final case class ReachableException(
    private val message: String = "Code not reachable",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Joos1WReachability {

  def reachable(ast: AST): Boolean = {
    ast match {
      case _ => throw ReachableException("Undefined reachability")
    }
  }

  def checkReachability(ast: AST): Unit = {}
}
