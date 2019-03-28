package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._

object MethodASM {
  def methodASM(ast: Option[AST]): ASM = {
    ast match {
      case Some(methodBody: MethodBody) =>
        if (methodBody.hasBody) methodASM(methodBody.leftChild) else ASM("")
      case Some(retAST: Return) =>
        val exprCode = astASM(Some(retAST.expr())).code
        ASM(s""";; return <expr>
               |$exprCode
               |ret
               |""".stripMargin)
      case Some(intAST: literals.IntegerLiteral) =>
        val intVal = intAST.integerValue
        ASM(s"mov eax, $intVal ;; integer literal")
      case Some(astList: ASTList) =>
        astList.fieldName match {
          case "block_statements" =>
            astList.children.foldLeft(ASM(""))((acc, ast) =>
              acc ++ methodASM(Some(ast)))
        }
      case Some(stmt: TopLevelIf) =>
        ExpressionASM.topLevelIfStatementASM(stmt)
    }
  }
}
