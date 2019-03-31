package compiler.joos1w.asm

import compiler.joos1w.Joos1WCodeGen
import compiler.joos1w.ast._
import compiler.joos1w.environment.environment
import compiler.joos1w.environment.types.{CustomType, StringType}

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
            ASM(s";; begin ${expr.firstExpr} + ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |pop ebx
                 |add eax, ebx
                 |;; end ${expr.firstExpr} + ${expr.secondExpr}
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
                 |;; end ${expr.firstExpr} - ${expr.secondExpr}
                 |""".stripMargin)
          case "*" =>
            ASM(s";; ${expr.firstExpr} * ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              ASM(s"""
                 |pop ebx
                 |imul eax, ebx
                 |;; end ${expr.firstExpr} * ${expr.secondExpr}
                 |""".stripMargin)
          case "/" =>
            ASM(s";; ${expr.firstExpr} / ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              // TODO something about the sign
              ASM(s"""
                 |mov ebx, eax
                 |pop eax
                 |mov edx, 0
                 |div ebx
                 |;; end ${expr.firstExpr} / ${expr.secondExpr}
                 |""".stripMargin)
          case "%" =>
            ASM(s";; ${expr.firstExpr} % ${expr.secondExpr}") ++
              expr1Code ++
              ASM("push eax") ++
              expr2Code ++
              // TODO something about the sign
              ASM(s"""
                     |mov ebx, eax
                     |pop eax
                     |mov edx, 0
                     |div ebx
                     |mov eax, edx ;; get remainder into eax from edx
                     |;; end ${expr.firstExpr} % ${expr.secondExpr}
                     |""".stripMargin)
          case "instanceof" =>
            val myCounter = incrementAndReturnCounter
            val rhs = environment.determineType(expr.secondExpr, expr.env)
            val rhsTypeEnv = rhs match {
              case rhs: StringType => rhs.env
              case rhs: CustomType => rhs.env
            }

            val classLabel = Joos1WCodeGen.classLabel(rhsTypeEnv)

            ASM(s";; ${expr.firstExpr} instanceof( ${expr.secondExpr} )") ++
              ASM(";; get left side instanceof") ++
              expr1Code ++
              ASM(s"""
                     |push ebx ;; store lhs pointer
                     |;; get instanceof right sides class into ebx
                     |mov ebx, ${classLabel}
                     |push ebx ;; save class pointer""".stripMargin)
            expr2Code ++
              ASM(s"""
                     |mov eax, ebx
                     |pop ebx ;; restore class pointer, now eax has pointer to lhs, ebx has pointer to rhs
                     |;; perform actual instance of, eax has lhs class, ebx has rhs class
                     |mov ecx, [ebx + 8] ;; get offset of subclass table for rhs
                     |mov edx, [eax + 4] ;; get offset to subclass table for lhs
                     |mov eax, 0xffffffff
                     |cmp eax, [ecx + edx] ;; check if rhs is subclass of lhs
                     |mov eax, 0xffffffff
                     |je .instanceof_subtype_check_pass${myCounter}
                     |mov eax, 0
                     |.instanceof_subtype_check_pass${myCounter}:""".stripMargin)

            ASM(s";; TODO fix instance of")
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
                   |cmp eax, 0xffffffff
                   |je .end_and${myCounter}""".stripMargin) ++
              expr2Code ++
              ASM(s".end_and${myCounter}:")

        }
    }
  }
}
