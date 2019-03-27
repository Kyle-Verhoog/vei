package compiler.joos1w

import ast.{AST, CompilationUnit}

case class ASMFile(fileName: String, src: String)

object ASM {
  def apply(rawCode: String): ASM = {
    new ASM(rawCode)
  }
}

class ASM(val rawCode: String) {
  def code: String = {
    rawCode
  }

  def ++(otherCode: ASM): ASM = {
    new ASM(rawCode ++ otherCode.code)
  }
}

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

  def astASM(ast: Option[AST]): ASM = {
    ASM("")
  }

  def astStaticIntTestASM(ast: Option[AST]): ASM = {
    ast match {
      case _ => ASM("")
    }
  }

  def astMainASM(ast: Option[AST]): ASM = {
    ASM("""global _start
      |_start:
      |""".stripMargin) ++
      astStaticIntTestASM(ast) ++
      ASM("""mov eax, 1
        |mov ebx, 7
        |int 0x80
        |""".stripMargin)
  }

  def genCode(asts: List[AST]): List[ASMFile] = {
    List(ASMFile(fileName(Some(asts.head)), astMainASM(Some(asts.head)).code)) ++
      asts.tail.map(ast => ASMFile(fileName(Some(ast)), astASM(Some(ast)).code))
  }
}
