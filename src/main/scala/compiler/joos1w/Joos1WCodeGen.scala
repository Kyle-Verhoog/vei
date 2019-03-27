package compiler.joos1w

import asm._
import ast._

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
        val pkgName = "todo.package".replaceAllLiterally(".", "_")
        val clsName = cls.identifier
        val clsId = s"${pkgName}_$clsName"
        val clsCode = astASM(Some(cls.getClassBody)).code
        ASM(s"""global $clsId
          |$clsId:
          |$clsCode
          |""".stripMargin)
      case Some(meth: MethodDeclaration) =>
        val pkgName = "todo.package".replaceAllLiterally(".", "_")
        val clsName = meth.env
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
        val methCode = astASM(Some(meth.body)).code
        ASM(s"""global $methId
               |$methId:
               |$methCode
               |""".stripMargin)
      case Some(methodBody: MethodBody) =>
        if (methodBody.hasBody) astASM(methodBody.leftChild) else ASM("")
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"""mov eax, $intVal ;; integer literal
               |""".stripMargin)
      case Some(expr: GeneralExpression) =>
        ExpressionASM.generalExpressionASM(expr)
      case Some(expr: UnaryExpression) =>
        ExpressionASM.unaryExpressionASM(expr)
      case Some(expr: ConditionalExpression) =>
        ExpressionASM.conditionalExpressionASM(expr)
      case Some(retAST: Return) =>
        val exprCode = astASM(Some(retAST.expr())).code
        ASM(s""";; return <expr>
             |$exprCode
             |ret
             |""".stripMargin)
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "block_statements" | "class_body_declarations" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ astASM(Some(ast)))
        }
      case _ => ASM(s"""nop
           |""".stripMargin)
    }
  }

  def astStaticIntTestASM(ast: Option[AST]): ASM = {
    ast match {
      case _ =>
        ASM(s"""
           |call todo_package_Basic_MethodDeclaration_modifiers_public_static_int_test__
           |""".stripMargin)
    }
  }

  // this method is called with the AST of the class containing the
  // public static int test() method.
  // find the class name and use it to invoke the test method
  def astMainASM(ast: Option[AST]): ASM = {
    val staticIntTestCode = astStaticIntTestASM(ast).code
    ASM(s"""global _start
      |extern todo_package_Basic_MethodDeclaration_modifiers_public_static_int_test__
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
