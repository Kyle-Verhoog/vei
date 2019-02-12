package compiler.joos1w.ast

final case class SemanticException(
    private val message: String = "Semantic error",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
