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
        val expr1Code = recurseMethod(Some(expr.firstExpr))
        val expr2Code = recurseMethod(Some(expr.secondExpr))
        op match {
          case "+" =>
            ASM(s";; ${expr.firstExpr} + ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |pop ebx
                 |add eax, ebx
                 |""".stripMargin)
          case "-" =>
            ASM(s";; ${expr.firstExpr} - ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |mov ebx, eax
                 |pop eax
                 |sub eax, ebx
                 |""".stripMargin)
          case "*" =>
            ASM(s";; ${expr.firstExpr} * ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |pop ebx
                 |imul eax, ebx
                 |""".stripMargin)
          case "/" =>
            ASM(s";; ${expr.firstExpr} / ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |mov ebx, eax
                 |pop eax
                 |mov edx, 0
                 |div ebx
                 |""".stripMargin)
          case "%"          => ASM(";; TODO %") // TODO
          case "instanceof" => ASM(";; TODO instanceof") // TODO
        }
      case _ => throw new MatchError(s"Expression match error")
    }
  }

  def unaryExpressionASM(expr: UnaryExpression,
                         recurseMethod: Option[AST] => ASM): ASM = {
    val exprCode = recurseMethod(Some(expr.subExpression))

    expr.operator match {
      case "!" =>
        ASM(";; ${expr.operator} ${expr.subExpression}") ++
          exprCode ++
          ASM(s"""
               |not eax
               |""".stripMargin)
      case "-" =>
        ASM(";; ${expr.operator} ${expr.subExpression}") ++
          exprCode ++
          ASM(s"""
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
        val expr1Code = recurseMethod(Some(expr.firstExpr))
        val expr2Code = recurseMethod(Some(expr.secondExpr))
        op match {
          case "!=" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} != ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
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
            ASM(s";; ${expr.firstExpr} == ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ; store expr2 value") ++
              expr1Code ++
              ASM(s"""
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
            ASM(s";; ${expr.firstExpr} >= ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
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
            ASM(s";; ${expr.firstExpr} > ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
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
            ASM(s";; ${expr.firstExpr} <= ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
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
            ASM(s";; ${expr.firstExpr} < ${expr.secondExpr}") ++
              expr2Code ++
              ASM("push eax ;; store expr2 value") ++
              expr1Code ++
              ASM(s"""
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
            ASM(s";; ${expr.firstExpr} && ${expr.secondExpr}") ++
              expr1Code ++
              ASM(s"""
                   |cmp eax, 0
                   |je .end_and${myCounter}
                 """.stripMargin) ++
              expr2Code ++
              ASM(s".end_and${myCounter}:")
          case "||" =>
            val myCounter = incrementAndReturnCounter
            ASM(s";; ${expr.firstExpr} || ${expr.secondExpr}") ++
              expr1Code ++
              ASM(s"""
                   |cmp eax, 1
                   |je .end_and${myCounter}""".stripMargin) ++
              expr2Code ++
              ASM(s".end_and${myCounter}:")

        }
    }
  }
}
