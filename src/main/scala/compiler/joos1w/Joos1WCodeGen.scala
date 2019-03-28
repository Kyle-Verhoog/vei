package compiler.joos1w

import asm._
import ast._
import compiler.joos1w.environment.{
  ClassEnvironment,
  MethodEnvironment,
  RootEnvironment,
  VariableEnvironment
}

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

  def astASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(ast: CompilationUnit) =>
        astASM(ast.typeDeclaration)
      case Some(ast: TypeDeclaration) =>
        astASM(ast.leftChild)
      case Some(cls: ClassDeclaration) =>
        val pkgName = cls.env
          .asInstanceOf[ClassEnvironment]
          .packageName
          .replaceAllLiterally(".", "_")
        val clsName = cls.identifier
        val clsId = s"${pkgName}_$clsName"
        val clsCode = astASM(Some(cls.getClassBody)).code
        ASM(s"""global $clsId
          |$clsId:
          |$clsCode
          |""".stripMargin)
      case Some(meth: MethodDeclaration) =>
        val env = meth.env.asInstanceOf[MethodEnvironment]
        val pkgName = env
          .asInstanceOf[MethodEnvironment]
          .findEnclosingClass()
          .packageName
          .replaceAllLiterally(".", "_")
        val clsName = env
          .findEnclosingClass()
          .myAst
          .asInstanceOf[ClassDeclaration]
          .identifier
        // TODO this should be the signature
        // val methName = meth.identifier ++ meth.toString.length.toString
        val methName = meth.toString // ++ meth.toString.hashCode.toString
          .replaceAllLiterally(".", "_")
          .replaceAllLiterally("(", "_")
          .replaceAllLiterally(")", "_")
          .replaceAllLiterally(" ", "_")
          .replaceAllLiterally("[", "_")
          .replaceAllLiterally("]", "_")
        val methId = s"${pkgName}_${clsName}_$methName"
        val methCode = MethodASM.methodASM(Some(meth.body)).code
        ASM(s"""global $methId
               |$methId:
               |push ebp  ;; push stack frame pointer
               |mov ebp, esp
               |sub esp, ${env.varCount * 4} ;; push the stack frame
               |$methCode
               |.method_end:
               |add esp, ${env.varCount * 4} ;; pop the frame
               |pop ebp  ;; pop stack frame pointer
               |ret
               |""".stripMargin)
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "class_body_declarations" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ astASM(Some(ast)))
          case s =>
            throw new MatchError(
              s"astASM match error on ASTList $s ${astList.parent.get.parent.get.toStrTree}")
        }
      case _ => ASM("nop")
      case Some(ast: AST) =>
        println(s"WARNING: FALLING THROUGH astASM on $ast")
        CommonASM.commonASM(Some(ast), astASM)
      case _ => throw new MatchError(s"methodAST match error on $ast")
    }
  }

  def astStaticIntTestASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(ast: CompilationUnit) =>
        astStaticIntTestASM(ast.typeDeclaration)
      case Some(ast: TypeDeclaration) =>
        astStaticIntTestASM(ast.leftChild)
      case Some(cls: ClassDeclaration) =>
        astStaticIntTestASM(Some(cls.getClassBody))
      case Some(meth: MethodDeclaration) =>
        if (meth.identifier == "test") {
          val pkgName = meth.env
            .asInstanceOf[MethodEnvironment]
            .findEnclosingClass()
            .packageName
          val clsName = meth.env
            .findEnclosingClass()
            .myAst
            .asInstanceOf[ClassDeclaration]
            .identifier
          val methName = meth.toString
            .replaceAllLiterally(".", "_")
            .replaceAllLiterally("(", "_")
            .replaceAllLiterally(")", "_")
            .replaceAllLiterally(" ", "_")
            .replaceAllLiterally("[", "_")
            .replaceAllLiterally("]", "_")
          val methId = s"${pkgName}_${clsName}_$methName"
          ASM(s"""
                 |extern $methId
                 |call $methId
                 |""".stripMargin)
        } else {
          ASM("")
        }
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "class_body_declarations" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ astStaticIntTestASM(Some(ast)))
        }
      case _ => ASM(";; TODO") // TODO
    }
  }

  // this method is called with the AST of the class containing the
  // public static int test() method.
  // find the class name and use it to invoke the test method
  def astMainASM(ast: Option[AST]): ASM = {
    val staticIntTestCode = astStaticIntTestASM(ast).code
    ASM(s"""global _start
      |_start:
      |$staticIntTestCode
      |mov ebx, eax
      |mov eax, 1
      |int 0x80
      |""".stripMargin)
  }

  def mkMainASMFile(ast: AST): ASMFile = {
    ASMFile("__main.s", astMainASM(Some(ast)).code)
  }

  def mkASMFile(ast: AST): ASMFile = {
    ASMFile(fileName(Some(ast)), astASM(Some(ast)).code)
  }

  def genCode(asts: List[AST]): List[ASMFile] = {
    mkMainASMFile(asts.head) :: asts.map(ast => mkASMFile(ast))
  }
}
