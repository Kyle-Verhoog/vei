package compiler.joos1w

import ast.{AST, CompilationUnit}

case class ASMFile(fileName: String, src: String)

object Joos1WCodeGen {
  def javaToASMFileName(name: String): String = {
    name
      .split("\\.java$")
      .head
      .replace("/", "_") ++ ".s"
  }

  def fileName(ast: Option[AST]): String = {
    ast match {
      case Some(ast: CompilationUnit) =>
        javaToASMFileName(ast.rawFileName)
      case Some(ast: AST) =>
        fileName(ast.leftChild) ++ fileName(ast.rightSibling)
      case None => ""
    }
  }

  def astCode(ast: Option[AST]): String = {
    ""
  }

  def astMainCode(ast: Option[AST]): String = {
    """
      |global _start
      |_start:
    """.stripMargin ++ astCode(ast)
  }

  def genCode(asts: List[AST]): List[ASMFile] = {
    List(ASMFile(fileName(Some(asts.head)), astMainCode(Some(asts.head)))) ++
      asts.tail.map(ast => ASMFile(fileName(Some(ast)), astCode(Some(ast))))
  }
}
