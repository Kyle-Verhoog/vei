package compiler.joos1w

import asm._
import ast._
import compiler.joos1w.environment.{
  ClassEnvironment,
  MethodEnvironment,
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
        val pkgName = meth.env
          .asInstanceOf[MethodEnvironment]
          .findEnclosingClass()
          .packageName
          .replaceAllLiterally(".", "_")
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
      case Some(stmt: TopLevelIf) =>
        ExpressionASM.topLevelIfStatementASM(stmt)
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
      case Some(v: LocalVariableDeclaration) =>
        val env = v.env.asInstanceOf[VariableEnvironment]
        val offset = env.offset.get
        val declCode = astASM(Some(v.variableDeclarator)).code
        ASM(s""";; ${v.ttype} ${v.name} = ${v.variableDeclarator}
             |$declCode
             |mov [esp + ${offset * 4}], eax
           """.stripMargin)
      case Some(vd: VariableDeclarator) =>
        astASM(Some(vd.expression))
      case _ => ASM(s"""nop
           |""".stripMargin)
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
