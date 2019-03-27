package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._

object ExpressionASM {
  def generalExpressionASM(expr: GeneralExpression): ASM = {
    expr.operation match {
      case Some(op) =>
        val expr1Code = Joos1WCodeGen.astASM(Some(expr.firstExpr)).code
        val expr2Code = Joos1WCodeGen.astASM(Some(expr.secondExpr)).code
        op match {
          case "+" =>
            ASM(s"""
                 |;; ${expr.firstExpr} + ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |pop ebx
                 |add eax, ebx
                 |""".stripMargin)
          case "-" =>
            ASM(s"""
                 |;; ${expr.firstExpr} - ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |pop ebx
                 |sub eax, ebx
                 |""".stripMargin)
          case "*" =>
            ASM(s"""
                 |;; ${expr.firstExpr} * ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |pop ebx
                 |imul eax, ebx
                 |""".stripMargin)
          case "/" =>
            ASM(s"""
                 |;; ${expr.firstExpr} / ${expr.secondExpr}
                 |$expr1Code
                 |push eax
                 |$expr2Code
                 |mov ebx, eax
                 |pop eax
                 |mov edx, 0
                 |div ebx
                 |""".stripMargin)
        }
      case None => ASM("")
    }
  }

  def unaryExpressionASM(expr: UnaryExpression): ASM = {
    val exprCode = Joos1WCodeGen.astASM(Some(expr.subExpression)).code

    expr.operator match {
      case "!" =>
        ASM(s""";; UNARY ${expr.operator} ${expr.subExpression}
               |$exprCode
               |not eax
               |""".stripMargin)
      case _ => throw new RuntimeException("TODO IMPLEMENT")
    }
  }

  def conditionalExpressionASM(expr: ConditionalExpression): ASM = {
    ASM("")
  }
}
