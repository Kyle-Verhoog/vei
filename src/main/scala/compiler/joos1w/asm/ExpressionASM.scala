package compiler.joos1w.asm

import compiler.joos1w.ast._

object ExpressionASM {
  var counter = 0

  def incrementAndReturnCounter: Int = {
    counter += 1
    counter
  }

  def generalExpressionASM(expr: GeneralExpression,
                           recurseMethod: Option[AST] => ASM): ASM = {
    expr.operation match {
      case Some(op) =>
        val expr1Code = recurseMethod(Some(expr.firstExpr)).code
        val expr2Code = recurseMethod(Some(expr.secondExpr)).code
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
                 |mov ebx, eax
                 |pop eax
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
          case "%"          => ASM(";TODO") // TODO
          case "instanceof" => ASM(";TODO") // TODO
        }
      case None => ASM("")
    }
  }

  def unaryExpressionASM(expr: UnaryExpression,
                         recurseMethod: Option[AST] => ASM): ASM = {
    val exprCode = recurseMethod(Some(expr.subExpression)).code

    expr.operator match {
      case "!" =>
        ASM(s""";; ${expr.operator} ${expr.subExpression}
               |$exprCode
               |not eax
               |""".stripMargin)
      case "-" =>
        ASM(s""";; ${expr.operator} ${expr.subExpression}
               |$exprCode
               |sub eax, eax
               |sub eax, eax
               |""".stripMargin)
      case s => throw new MatchError(s"TODO IMPLEMENT $s")
    }
  }

  def conditionalExpressionASM(expr: ConditionalExpression,
                               recurseMethod: Option[AST] => ASM): ASM = {
    expr.operator match {
      case op =>
        val expr1Code = recurseMethod(Some(expr.firstExpr)).code
        val expr2Code = recurseMethod(Some(expr.secondExpr)).code
        op match {
          case "!=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} != ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax ; expr 2 result in ebx
                   |push ebx ;; store expr2 value
                   |$expr1Code
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jne .end_not_equal${myCounter}
                   |mov ebx, 0
                   |.end_not_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "==" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} != ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax ; expr 2 result in ebx
                   |push ebx ;; store expr2 value
                   |$expr1Code
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 0
                   |jne .end_equal_equal${myCounter}
                   |mov ebx, 1
                   |.end_equal_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case ">=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} >= ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |push ebx ;; store expr2 value
                   |$expr1Code
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jge .end_greater_equal${myCounter}
                   |mov ebx, 0
                   |.end_greater_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case ">" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} > ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |push ebx ;; store expr2 value
                   |$expr1Code
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jg .end_greater${myCounter}
                   |mov ebx, 0
                   |.end_greater${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "<=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} <= ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |push ebx ;; store expr2 value
                   |$expr1Code
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jle .end_less_equal${myCounter}
                   |mov ebx, 0
                   |.end_less_equal${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "<" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} < ${expr.secondExpr}
                   |$expr2Code
                   |mov ebx, eax
                   |push ebx ;; store expr2 value
                   |$expr1Code
                   |pop ebx ;; restore expr2 value
                   |cmp eax, ebx
                   |mov ebx, 1
                   |jl .end_less${myCounter}
                   |mov ebx, 0
                   |.end_less${myCounter}:
                   |mov eax, ebx
                   |""".stripMargin)
          case "&&" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} && ${expr.secondExpr}
                   |$expr1Code
                   |cmp eax, 0
                   |je .end_and${myCounter}
                   |$expr2Code
                   |.end_and${myCounter}:
                   |""".stripMargin)
          case "||" =>
            val myCounter = incrementAndReturnCounter
            ASM(s"""
                   |;; ${expr.firstExpr} || ${expr.secondExpr}
                   |$expr1Code
                   |cmp eax, 1
                   |je .end_and${myCounter}
                   |$expr2Code
                   |.end_and${myCounter}:
                   |""".stripMargin)

        }
    }
  }
}
